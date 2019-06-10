module Main exposing (Flags, Model, Msg(..), Path, findFirstOccIndex, foldInlines, init, main, subscriptions, update, view)

import Browser exposing (document)
import Browser.Events exposing (onResize)
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Region as Region
import Html as Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Json.Decode as D
import Json.Encode as E
import Markdown exposing (..)
import Markdown.Block as Block exposing (..)
import Markdown.Config exposing (defaultOptions)
import Markdown.Inline as Inline exposing (..)
import Parser exposing (..)
import Random


type alias Model b =
    { seed : Random.Seed
    , maxWidth : Int
    , selected : Maybe Selection
    , cursorPos : Maybe Int
    , setSelection : Maybe E.Value
    , rawInput : String
    , parsedInput : List (Block b InlineStyle)
    }


type alias Path =
    String


subscriptions model =
    Sub.batch [ onResize WinResize ]


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = ---------------------------
      -- Textarea Manipulation --
      ---------------------------
      TextInput CustomInput
    | NewSelection Selection
    | SetSelection
      ----------
      -- Misc --
      ----------
    | WinResize Int Int
    | NoOp


type alias Flags =
    { currentTime : Int
    , width : Int
    , height : Int
    }


init : Flags -> ( Model b, Cmd Msg )
init flags =
    ( { seed = Random.initialSeed flags.currentTime
      , maxWidth = flags.width
      , selected = Nothing
      , cursorPos = Nothing
      , setSelection = Nothing
      , rawInput = ""
      , parsedInput = []
      }
    , Cmd.none
    )


update msg model =
    case msg of
        TextInput { selection, valueStr } ->
            ( { model
                | rawInput = valueStr
                , parsedInput =
                    Block.parse (Just { defaultOptions | softAsHardLineBreak = True })
                        valueStr
                        |> List.map addCustomStyles
                , cursorPos =
                    if selection.start == selection.finish then
                        Just selection.start

                    else
                        Nothing
              }
            , Cmd.none
            )

        NewSelection s ->
            ( { model
                | selected =
                    if s.start == s.finish then
                        Nothing

                    else
                        Just s
                , cursorPos =
                    if s.start == s.finish then
                        Just s.start

                    else
                        Nothing
              }
            , Cmd.none
            )

        SetSelection ->
            ( model, Cmd.none )

        WinResize width height ->
            ( { model | maxWidth = width }
            , Cmd.batch
                []
            )

        NoOp ->
            ( model, Cmd.none )


view model =
    { title = ""
    , body =
        [ layout []
            (column
                [ padding 15
                , spacing 15
                , width (px (min 1000 model.maxWidth))
                , centerX
                ]
                [ selectionInfoView model
                , customTextArea
                    [ width fill ]
                    model.setSelection
                    500
                    model.rawInput
                , blocksToElements (\_ -> NoOp) model.parsedInput
                    |> column
                        [ width fill
                        , spacing 15
                        , Font.family
                            [ Font.typeface "Lato"
                            , Font.sansSerif
                            ]
                        ]
                , paragraph []
                    [ text (Debug.toString model.parsedInput) ]
                , paragraph []
                    [ text (Debug.toString <| Parser.run inlineStylesOffsets model.rawInput) ]
                ]
            )
        ]
    }


selectionInfoView model =
    let
        cursorView =
            case model.cursorPos of
                Nothing ->
                    "Nothing"

                Just n ->
                    String.fromInt n

        selectionView =
            case model.selected of
                Nothing ->
                    "Nothing"

                Just { start, finish, sel } ->
                    "start: "
                        ++ String.fromInt start
                        ++ ", stop: "
                        ++ String.fromInt finish
    in
    row
        [ spacing 15 ]
        [ text <| "cursor: " ++ cursorView
        , text <| "selection bounds: " ++ selectionView
        ]



-------------------------------------------------------------------------------
---------------------
-- Custom textarea --
---------------------


type alias Selection =
    { start : Int
    , finish : Int
    , sel : String
    }


type alias CustomInput =
    { selection : Selection
    , valueStr : String
    }


decodeCustomInput : D.Decoder CustomInput
decodeCustomInput =
    D.map2 CustomInput
        (D.at [ "target", "selection" ]
            (D.map3 Selection
                (D.field "start" D.int)
                (D.field "finish" D.int)
                (D.field "sel" D.string)
            )
        )
        (D.at [ "target", "value" ] D.string)


onCustomInput : (CustomInput -> msg) -> Html.Attribute msg
onCustomInput tagger =
    HtmlEvents.stopPropagationOn "Input" (D.map alwaysStop (D.map tagger decodeCustomInput))


alwaysStop : a -> ( a, Bool )
alwaysStop x =
    ( x, True )


customTextArea :
    List (Element.Attribute Msg)
    -> Maybe E.Value
    -> Int
    -> String
    -> Element.Element Msg
customTextArea attrs setSelection height rawInput =
    el attrs
        (html <|
            Html.node "custom-textarea"
                ([ HtmlEvents.on "Selection" decodeSelection
                 , onCustomInput TextInput
                 ]
                    ++ (case setSelection of
                            Just selection ->
                                [ HtmlAttr.property "selection" selection ]

                            Nothing ->
                                []
                       )
                )
                [ Html.textarea
                    [ HtmlAttr.style "font-family" "Arial"
                    , HtmlAttr.style "font-size" "16px"
                    , HtmlAttr.cols
                        (if height == 300 then
                            70

                         else
                            60
                        )
                    , HtmlAttr.style "height" (String.fromInt height ++ "px")
                    , HtmlAttr.style "spellcheck" "false"
                    , HtmlAttr.style "background-color" "Beige"
                    , HtmlAttr.value rawInput
                    ]
                    []
                ]
        )


encodeSelection : Int -> Int -> E.Value
encodeSelection start stop =
    E.object
        [ ( "start", E.int start )
        , ( "stop", E.int stop )
        ]


decodeSelection : D.Decoder Msg
decodeSelection =
    D.at [ "target", "selection" ]
        (D.map3 Selection
            (D.field "start" D.int)
            (D.field "finish" D.int)
            (D.field "sel" D.string)
            |> D.map NewSelection
        )



-------------------------------------------------------------------------------
-----------------------------------------
-- Custom markdown parser and renderer --
-----------------------------------------


renderMarkdown : String -> (String -> msg) -> Element msg
renderMarkdown s downloadHandler =
    Block.parse (Just { defaultOptions | softAsHardLineBreak = True }) s
        |> blocksToElements downloadHandler
        |> column
            [ width fill
            , spacing 15
            , Font.family
                [ Font.typeface "Lato"
                , Font.sansSerif
                ]
            ]


blocksToElements : (String -> msg) -> List (Block b InlineStyle) -> List (Element msg)
blocksToElements downloadHandler blocks =
    List.map (blockToElement downloadHandler 0) blocks


blockToElement : (String -> msg) -> Int -> Block b InlineStyle -> Element msg
blockToElement downloadHandler offset block =
    case block of
        BlankLine s ->
            Element.none

        ThematicBreak ->
            el
                [ width fill
                , Border.color (rgb255 127 127 127)
                , Border.width 1
                ]
                Element.none

        Heading raw level inlines ->
            headings downloadHandler raw level inlines

        CodeBlock _ raw ->
            el []
                (html <|
                    Html.pre []
                        [ Html.text raw ]
                )

        Paragraph raw inlines ->
            paragraph
                [ Font.family
                    [ Font.typeface "times" ]
                , Font.size 18
                ]
                (List.concatMap parseCustomStyles inlines
                    |> List.concatMap (inlinesToElements downloadHandler [])
                )

        BlockQuote blocks ->
            column
                [ spacing 15 ]
                (List.map (blockToElement downloadHandler offset) blocks)

        List listblock llistBlocks ->
            let
                bullet off =
                    if off == 0 then
                        "•"

                    else if off == 1 then
                        "◦"

                    else
                        "‣"

                liView ( i, bs ) =
                    case bs of
                        [] ->
                            []

                        (List _ _) :: _ ->
                            List.map (blockToElement downloadHandler (offset + 1)) bs

                        _ ->
                            [ row
                                [ width fill
                                , spacing 5
                                ]
                                [ case listblock.type_ of
                                    Unordered ->
                                        el [ alignTop ] (text <| bullet offset)

                                    Ordered start ->
                                        el [ alignTop ] (text <| String.fromInt (start + i) ++ ". ")
                                , paragraph [] (List.map (blockToElement downloadHandler (offset + 1)) bs)
                                ]
                            ]
            in
            column
                [ spacing 10
                , paddingXY 0 10
                ]
                (List.concatMap liView (List.indexedMap Tuple.pair llistBlocks))

        PlainInlines inlines ->
            paragraph
                []
                (List.concatMap parseCustomStyles inlines
                    |> List.concatMap (inlinesToElements downloadHandler [])
                )

        Block.Custom b llistBlocks ->
            Element.none


headings downloadHandler raw level inlines =
    let
        headingStyles =
            Dict.fromList
                [ ( 1
                  , [ Font.size 45 ]
                  )
                , ( 2
                  , [ Font.size 35 ]
                  )
                , ( 3
                  , [ Font.size 30 ]
                  )
                , ( 4
                  , [ Font.size 25 ]
                  )
                , ( 5
                  , [ Font.size 15 ]
                  )
                , ( 6
                  , [ Font.size 10 ]
                  )
                ]
    in
    paragraph
        ([ Region.heading level
         , Font.color (rgb255 0 0 0)
         , Font.family
            [ Font.typeface "Crimson Text"
            , Font.serif
            ]
         ]
            ++ (Dict.get level headingStyles
                    |> Maybe.withDefault []
               )
        )
        (List.concatMap parseCustomStyles inlines
            |> List.concatMap (inlinesToElements downloadHandler [])
        )


parseCustomStyles : Inline InlineStyle -> List (Inline InlineStyle)
parseCustomStyles inline =
    case inline of
        Text s ->
            case Parser.run inlineStyles s of
                Ok res ->
                    List.foldr
                        (\is acc ->
                            case is of
                                Regular r ->
                                    Text r :: acc

                                styled ->
                                    Inline.Custom styled [] :: acc
                        )
                        []
                        res

                _ ->
                    [ Text s ]

        _ ->
            [ inline ]


inlinesToElements : (String -> msg) -> List (Attribute msg) -> Inline InlineStyle -> List (Element msg)
inlinesToElements downloadHandler attrs inline =
    case inline of
        Text s ->
            [ el attrs (text s) ]

        HardLineBreak ->
            [ el attrs (html <| Html.br [] []) ]

        CodeInline s ->
            [ el attrs (text s) ]

        Link url mbTitle inlines ->
            if String.contains "Documents/" url then
                [ el
                    [ mouseOver
                        [ Font.color (rgb255 0 0 127)
                        ]
                    , Font.underline
                    , Font.color (rgb255 0 0 200)
                    , pointer
                    , Events.onClick (downloadHandler url)
                    ]
                    (text <| Inline.extractText inlines)
                ]

            else
                [ (if String.startsWith "/" url then
                    link

                   else
                    newTabLink
                  )
                    (attrs
                        ++ [ mouseOver
                                [ Font.color (rgb255 0 0 127)
                                ]
                           , Font.underline
                           , Font.color (rgb255 0 0 200)
                           ]
                    )
                    { url = url
                    , label = text <| Inline.extractText inlines
                    }
                ]

        Image url mbTitle inlines ->
            [ image
                (attrs ++ [ width fill ])
                { src = url
                , description = Inline.extractText inlines
                }
            ]

        HtmlInline s _ _ ->
            [ el attrs (text s) ]

        Emphasis n inlines ->
            let
                attrs_ =
                    attrs
                        ++ (if n == 1 then
                                [ Font.italic ]

                            else if n == 2 then
                                [ Font.bold ]

                            else if n == 3 then
                                [ Font.italic
                                , Font.bold
                                ]

                            else
                                []
                           )
            in
            List.concatMap parseCustomStyles inlines
                |> List.concatMap (inlinesToElements downloadHandler attrs_)

        Inline.Custom (Styled styled) inlines ->
            [ styledToElement attrs styled ]

        Inline.Custom (Regular _) _ ->
            []



-------------------------------------------------------------------------------
-------------------------
-- Inline style parser --
-------------------------


type InlineStyle
    = Styled
        { styled : String
        , attrs : List StyleAttribute
        }
    | Regular String


type StyleAttribute
    = Font String
    | FontSize Int
    | Color String


styledToElement : List (Attribute msg) -> { styled : String, attrs : List StyleAttribute } -> Element msg
styledToElement attrs_ { styled, attrs } =
    el (List.concatMap (styleAttributeToElementAttr attrs_) attrs)
        (text styled)


styleAttributeToElementAttr : List (Attribute msg) -> StyleAttribute -> List (Attribute msg)
styleAttributeToElementAttr attrs attr =
    case attr of
        Font s ->
            attrs
                ++ [ Font.family
                        [ Font.typeface s ]
                   ]

        FontSize n ->
            attrs ++ [ Font.size n ]

        Color s ->
            attrs ++ [ Font.color <| rgb255 255 0 0 ]


inlineStyles : Parser (List InlineStyle)
inlineStyles =
    let
        helper styles =
            oneOf
                [ succeed (\style -> Loop (style :: styles))
                    |= inlineStyle
                    |> backtrackable
                , symbol "["
                    |> Parser.map (\_ -> Loop (Regular "[" :: styles))
                , succeed (\reg -> Loop (Regular reg :: styles))
                    |= (chompUntil "["
                            |> getChompedString
                       )
                    |> backtrackable
                , succeed
                    (\reg ->
                        Done
                            (List.reverse
                                (Regular reg :: styles)
                                |> concatRegs "" []
                            )
                    )
                    |= (chompUntilEndOr "\n"
                            |> getChompedString
                       )
                ]

        concatRegs acc res xs =
            case xs of
                [] ->
                    if acc == "" then
                        List.reverse res

                    else
                        List.reverse <| Regular acc :: res

                (Styled s) :: xs_ ->
                    if acc == "" then
                        concatRegs "" (Styled s :: res) xs_

                    else
                        concatRegs "" (Styled s :: Regular acc :: res) xs_

                (Regular r) :: xs_ ->
                    concatRegs (acc ++ r) res xs_
    in
    loop [] helper


inlineStylesOffsets : Parser (Dict ( Int, Int ) InlineStyle)
inlineStylesOffsets =
    let
        helper offsets =
            oneOf
                [ succeed (\offset -> Loop (offset :: offsets))
                    |= inlineStyleOffsets
                    |> backtrackable
                , succeed (Loop offsets)
                    |. chompUntil "["
                    |> backtrackable
                , succeed
                    (Done offsets)
                    |. chompUntilEndOr "\n"
                ]
    in
    loop [] helper
        |> Parser.map Dict.fromList


inlineStyleOffsets : Parser ( ( Int, Int ), InlineStyle )
inlineStyleOffsets =
    succeed
        (\start styled stop ->
            ( ( start, stop ), styled )
        )
        |= getOffset
        |= inlineStyle
        |= getOffset


inlineStyle : Parser InlineStyle
inlineStyle =
    succeed (\s attrs -> Styled { styled = s, attrs = attrs })
        |. symbol "["
        |= (chompUntil "]"
                |> getChompedString
           )
        |. symbol "]"
        |. spaces
        |. symbol "{"
        |. spaces
        |. keyword "style"
        |. spaces
        |. symbol "|"
        |. spaces
        |= styleAttributes


styleAttributes : Parser (List StyleAttribute)
styleAttributes =
    let
        helper attrs =
            oneOf
                [ succeed (\attr -> Loop (attr :: attrs))
                    |= styleAttribute
                    |. spaces
                    |. symbol ","
                    |. spaces
                    |> backtrackable
                , succeed (\attr -> Done (List.reverse <| attr :: attrs))
                    |= styleAttribute
                    |. spaces
                    |. symbol "}"
                ]
    in
    loop [] helper


styleAttribute : Parser StyleAttribute
styleAttribute =
    oneOf
        [ attribute Font "police" value
            |> backtrackable
        , attribute FontSize "taille" int
            |> backtrackable
        , attribute Color "couleur" value
        ]


attribute : (a -> b) -> String -> Parser a -> Parser b
attribute sAttr name valueParser =
    succeed sAttr
        |. keyword name
        |. spaces
        |. symbol ":"
        |. spaces
        |= valueParser


value : Parser String
value =
    chompWhile
        (\c ->
            ((c /= ',')
                || (c /= '}')
                || (c /= ' ')
            )
                && Char.isAlphaNum c
        )
        |> getChompedString
        |> Parser.andThen
            (\s ->
                if s == "" then
                    problem "empty value"

                else
                    succeed s
            )



------------------------------------------------------------------------------
------------------------------------
-- Custom inlines positions index --
------------------------------------
--type Block b i
--


addCustomStyles : Block b InlineStyle -> Block b InlineStyle
addCustomStyles block =
    case block of
        BlankLine s ->
            BlankLine s

        ThematicBreak ->
            ThematicBreak

        Heading raw level inlines ->
            Heading raw level (List.concatMap parseCustomStyles inlines)

        CodeBlock cb raw ->
            CodeBlock cb raw

        Paragraph raw inlines ->
            Paragraph raw (List.concatMap parseCustomStyles inlines)

        BlockQuote blocks ->
            BlockQuote (List.map addCustomStyles blocks)

        List listblock llistBlocks ->
            List listblock (List.map (List.map addCustomStyles) llistBlocks)

        PlainInlines inlines ->
            PlainInlines (List.concatMap parseCustomStyles inlines)

        Block.Custom b blocks ->
            Block.Custom b (List.map addCustomStyles blocks)


indexCustomInlines rawInput parsedInput =
    List.foldr (\b acc -> foldInlines indexCustomInline acc b) ( rawInput, Dict.empty ) parsedInput


indexCustomInline : Inline InlineStyle -> ( String, Dict ( Int, Int ) String ) -> ( String, Dict ( Int, Int ) String )
indexCustomInline inline ( source, indexes ) =
    case inline of
        Inline.Custom (Styled s) inlines ->
            getIndex s.styled ( source, indexes )

        _ ->
            ( source, indexes )


getIndex : String -> ( String, Dict ( Int, Int ) String ) -> ( String, Dict ( Int, Int ) String )
getIndex styled ( source, indexes ) =
    case Debug.log "" (findFirstOccIndex styled source) of
        ( Just index, l, remaining ) ->
            ( remaining, Dict.insert ( index, index + l ) styled indexes )

        ( Nothing, _, _ ) ->
            ( source, indexes )


foldInlines :
    (Inline i
     -> a
     -> a
    )
    -> a
    -> Block b i
    -> a
foldInlines f acc block =
    case block of
        Paragraph rawText inlines ->
            List.foldr f acc inlines

        Heading rawText level inlines ->
            List.foldr f acc inlines

        PlainInlines inlines ->
            List.foldr f acc inlines

        BlockQuote blocks ->
            List.foldr (\b acc_ -> foldInlines f acc_ b) acc blocks

        List listBlock items ->
            List.foldr
                (\item acc_ ->
                    List.foldr (\b acc__ -> foldInlines f acc__ b)
                        acc_
                        item
                )
                acc
                items

        Block.Custom customBlock blocks ->
            List.foldr (\b acc_ -> foldInlines f acc_ b) acc blocks

        _ ->
            acc



--foldInlinesHelp :
--    (Inline i
--     -> a
--     -> a
--    )
--    -> a
--    -> Block b i
--    -> a
--foldInlinesHelp function acc block =
--    case block of
--        Paragraph rawText inlines ->
--            List.foldr function acc inlines
--        Heading rawText level inlines ->
--            List.foldr function acc inlines
--        PlainInlines inlines ->
--            List.foldr function acc inlines
--        _ ->
--            acc
--foldBlock :
--    (Block b i
--     -> a
--     -> a
--    )
--    -> a
--    -> Block b i
--    -> a
--foldBlock function acc block =
--    case block of
--        BlockQuote blocks ->
--            List.foldr function acc blocks
--        List listBlock items ->
--            List.foldr (\b acc_ -> List.foldr function acc_ b) acc items
--        Block.Custom customBlock blocks ->
--            List.foldr function acc blocks
--        _ ->
--            function block acc
-------------------------------------------------------------------------------
----------
-- Misc --
----------


findFirstOccIndex : String -> String -> ( Maybe Int, Int, String )
findFirstOccIndex subStr source =
    let
        n =
            String.length subStr

        helper remaining offset =
            if remaining == "" then
                ( Nothing, n, "" )

            else if String.left n remaining == subStr then
                ( Just offset, n, String.dropLeft n remaining )

            else
                helper (String.dropLeft 1 remaining) (offset + 1)
    in
    helper source 0
