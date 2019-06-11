module Main exposing (Flags, Model, Msg(..), Path, findFirstOccIndex, foldInlines, init, main, subscriptions, update, view)

import Browser exposing (document)
import Browser.Events exposing (onResize)
import Dict exposing (..)
import Dict.Extra exposing (find)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Region as Region
import Hex exposing (fromString)
import Html as Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Json.Decode as D
import Json.Encode as E
import List.Extra exposing (uniqueBy)
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
    , stylesIndexes : Dict String InlineStyle
    , currentStyle : Maybe ( InlineStyleBounds, InlineStyle )
    , openedWidget : Maybe Widget
    }


type alias Path =
    String


type Widget
    = FontColorPicker
    | BackgroundColorPicker


subscriptions model =
    Sub.batch
        [ onResize WinResize
        , case model.openedWidget of
            Just FontColorPicker ->
                Browser.Events.onMouseDown (outsideTargetHandler "fontColorPicker" Close)

            Just BackgroundColorPicker ->
                Browser.Events.onMouseDown (outsideTargetHandler "backgroundColorPicker" Close)

            _ ->
                Sub.none
        ]


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
      ----------------
      -- Set styles --
      ----------------
    | SetTextColor String
    | SetBackgroundColor String
      ----------
      -- Misc --
      ----------
    | WinResize Int Int
    | OpenFontColorPicker
    | OpenBackgroundColorPicker
    | Close
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
      , rawInput = sampleString
      , parsedInput = []
      , stylesIndexes = Dict.empty
      , currentStyle = Nothing
      , openedWidget = Nothing
      }
    , Cmd.none
    )


update msg model =
    case msg of
        ---------------------------
        -- Textarea Manipulation --
        ---------------------------
        TextInput { selection, valueStr } ->
            let
                rawInput =
                    valueStr

                parsedInput =
                    Block.parse (Just { defaultOptions | softAsHardLineBreak = True })
                        valueStr
                        |> List.map addCustomStyles

                stylesIndexes =
                    case Parser.run inlineStylesOffsets valueStr of
                        Ok indexes ->
                            indexes

                        _ ->
                            Dict.empty

                selected =
                    if selection.start == selection.stop then
                        Nothing

                    else
                        Just selection

                cursorPos =
                    if selection.start == selection.stop then
                        Just selection.start

                    else
                        Nothing

                currentStyle =
                    findInlineStyleFromCursorPos stylesIndexes selection
            in
            ( { model
                | rawInput = rawInput
                , parsedInput = parsedInput
                , stylesIndexes = stylesIndexes
                , selected = selected
                , cursorPos = cursorPos
                , currentStyle = currentStyle
              }
            , Cmd.none
            )

        NewSelection s ->
            let
                isCursor =
                    s.start == s.stop

                currentStyle =
                    if isCursor then
                        findInlineStyleFromCursorPos model.stylesIndexes s

                    else
                        Nothing

                setSelection =
                    Maybe.map
                        (\( { styleStart, styleStop }, _ ) ->
                            encodeSelection styleStart styleStop
                        )
                        currentStyle
            in
            ( { model
                | selected =
                    case currentStyle of
                        Just ( { styleStart, styleStop }, _ ) ->
                            Just (Selection styleStart styleStop)

                        Nothing ->
                            Just s
                , cursorPos =
                    if isCursor then
                        Just s.start

                    else
                        Nothing
                , currentStyle = currentStyle
                , setSelection =
                    Maybe.map
                        (\( { styleStart, styleStop }, _ ) ->
                            encodeSelection styleStart styleStop
                        )
                        currentStyle
              }
            , Cmd.none
            )

        SetSelection ->
            ( { model
                | setSelection =
                    Maybe.map
                        (\( { styleStart, styleStop }, _ ) ->
                            encodeSelection styleStart styleStop
                        )
                        model.currentStyle
              }
            , Cmd.batch
                []
            )

        ----------------
        -- Set styles --
        ----------------
        SetTextColor color ->
            case model.currentStyle of
                Nothing ->
                    ( insertStyle { model | openedWidget = Nothing } [ Color color ]
                    , Cmd.none
                    )

                Just cs ->
                    ( updateStyle { model | openedWidget = Nothing } cs [ Color color ]
                    , Cmd.none
                    )

        SetBackgroundColor color ->
            ( model, Cmd.none )

        ----------
        -- Misc --
        ----------
        WinResize width height ->
            ( { model | maxWidth = width }
            , Cmd.batch
                []
            )

        OpenFontColorPicker ->
            ( { model
                | openedWidget = Just FontColorPicker
              }
            , Cmd.none
            )

        OpenBackgroundColorPicker ->
            ( { model
                | openedWidget = Just BackgroundColorPicker
              }
            , Cmd.none
            )

        Close ->
            ( { model
                | openedWidget = Nothing
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


findInlineStyleFromCursorPos stylesIndexes s =
    Dict.Extra.find
        (\k _ -> cursorInBounds s.start k)
        stylesIndexes
        |> Maybe.map (Tuple.mapFirst stringToInlineStyleBounds)
        |> Maybe.andThen
            (\( mbBounds, is ) ->
                case mbBounds of
                    Nothing ->
                        Nothing

                    Just b ->
                        Just ( b, is )
            )


insertStyle : Model b -> List StyleAttribute -> Model b
insertStyle model newStyleAttrs =
    case model.selected of
        Just ({ start, stop } as sel) ->
            let
                newStyle =
                    Styled
                        { styled = String.slice start stop model.rawInput
                        , attrs = newStyleAttrs
                        }

                newStyleStr =
                    inlineStyleToString newStyle

                newSelection =
                    case Parser.run inlineStyleOffsets newStyleStr of
                        Ok ( { styleStart, styleStop }, _ ) ->
                            Selection (stop + styleStart) (stop + styleStop)

                        _ ->
                            sel

                prefix =
                    String.left start model.rawInput

                suffix =
                    String.dropLeft stop model.rawInput

                newValueStr =
                    prefix ++ newStyleStr ++ suffix
            in
            update (TextInput (CustomInput newSelection newValueStr)) model
                |> Tuple.first

        _ ->
            model


updateStyle : Model b -> ( InlineStyleBounds, InlineStyle ) -> List StyleAttribute -> Model b
updateStyle model ( { styleStart, styleStop }, cs ) newStyleAttrs =
    let
        newStyle =
            combineStyles cs newStyleAttrs

        newAttrStr =
            attrsToString newStyleAttrs

        prefix =
            String.left styleStart model.rawInput

        suffix =
            String.dropLeft styleStop model.rawInput

        newValueStr =
            prefix ++ newAttrStr ++ suffix
    in
    update (TextInput (CustomInput (Selection styleStart styleStop) newValueStr)) model
        |> Tuple.first


combineStyles : InlineStyle -> List StyleAttribute -> InlineStyle
combineStyles current new =
    case current of
        Styled cs ->
            Styled { cs | attrs = uniqueBy styleAttrsCat <| new ++ cs.attrs }

        _ ->
            current


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
                , colorPicker
                    "fontColorPicker"
                    (model.openedWidget == Just FontColorPicker)
                    (Maybe.andThen
                        (\( _, is ) ->
                            extractAttr
                                (\a ->
                                    case a of
                                        Color c ->
                                            Just c

                                        _ ->
                                            Nothing
                                )
                                is
                        )
                        model.currentStyle
                    )
                    OpenFontColorPicker
                    SetTextColor
                    "Set text color"
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

                Just { start, stop } ->
                    "start: "
                        ++ String.fromInt start
                        ++ ", stop: "
                        ++ String.fromInt stop
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
    , stop : Int
    }


type alias CustomInput =
    { selection : Selection
    , valueStr : String
    }


decodeCustomInput : D.Decoder CustomInput
decodeCustomInput =
    D.map2 CustomInput
        (D.at [ "target", "selection" ]
            (D.map2 Selection
                (D.field "start" D.int)
                (D.field "stop" D.int)
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
        (D.map2 Selection
            (D.field "start" D.int)
            (D.field "stop" D.int)
            --(D.field "sel" D.string)
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
                    [ Font.typeface "Times New Roman"
                    , Font.serif
                    ]
                , Font.size 18
                ]
                (List.concatMap (inlinesToElements downloadHandler []) inlines)

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
                                , paragraph
                                    [ Font.family
                                        [ Font.typeface "Times New Roman"
                                        , Font.serif
                                        ]
                                    , Font.size 18
                                    ]
                                    (List.map (blockToElement downloadHandler (offset + 1)) bs)
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
                (List.concatMap (inlinesToElements downloadHandler []) inlines)

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
        (List.concatMap (inlinesToElements downloadHandler []) inlines)


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

        Emphasis level inlines ->
            [ Emphasis level (List.concatMap parseCustomStyles inlines) ]

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
            List.concatMap (inlinesToElements downloadHandler attrs_) inlines

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


extractAttr : (StyleAttribute -> Maybe a) -> InlineStyle -> Maybe a
extractAttr p is =
    case is of
        Regular _ ->
            Nothing

        Styled { attrs } ->
            List.filterMap p attrs
                |> List.head


type StyleAttribute
    = Font String
    | FontSize Int
    | Color String


styleAttrsCat : StyleAttribute -> String
styleAttrsCat sa =
    case sa of
        Font _ ->
            "Font"

        FontSize _ ->
            "FontSize"

        Color _ ->
            "Color"


inlineStyleToString : InlineStyle -> String
inlineStyleToString is =
    case is of
        Styled { styled, attrs } ->
            "[ " ++ styled ++ " ]" ++ attrsToString attrs

        _ ->
            ""


attrsToString : List StyleAttribute -> String
attrsToString attrs =
    let
        attrToStr attr =
            case attr of
                Font s ->
                    "font: " ++ s

                FontSize n ->
                    "size: " ++ String.fromInt n

                Color c ->
                    "color: " ++ c
    in
    List.map attrToStr attrs
        |> String.join ", "
        |> (\res -> "{style| " ++ res ++ "}")


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
            attrs
                ++ [ Dict.get s webColors
                        |> Maybe.withDefault "000000"
                        |> hexToColor
                        |> Font.color
                   ]


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


type alias InlineStyleBounds =
    { bodyStart : Int
    , bodyStop : Int
    , styleStart : Int
    , styleStop : Int
    }


inlineStyleBoundsToString : InlineStyleBounds -> String
inlineStyleBoundsToString { bodyStart, bodyStop, styleStart, styleStop } =
    String.fromInt bodyStart
        ++ "-"
        ++ String.fromInt bodyStop
        ++ "-"
        ++ String.fromInt styleStart
        ++ "-"
        ++ String.fromInt styleStop


cursorInBounds : Int -> String -> Bool
cursorInBounds cursorPos bounds =
    case stringToInlineStyleBounds bounds of
        Just { styleStart, styleStop } ->
            (cursorPos >= styleStart) && (cursorPos < styleStop)

        _ ->
            False


stringToInlineStyleBounds : String -> Maybe InlineStyleBounds
stringToInlineStyleBounds s =
    case
        String.split "-" s
            |> List.filterMap String.toInt
    of
        bodyStart :: bodyStop :: styleStart :: styleStop :: [] ->
            Just <| InlineStyleBounds bodyStart bodyStop styleStart styleStop

        _ ->
            Nothing


inlineStylesOffsets : Parser (Dict String InlineStyle)
inlineStylesOffsets =
    let
        helper offsets =
            oneOf
                [ succeed (\offset -> Loop (offset :: offsets))
                    |= inlineStyleOffsets
                    |> backtrackable
                , succeed (Loop offsets)
                    |. symbol "["
                , succeed (Loop offsets)
                    |. chompUntil "["
                    |> backtrackable
                , succeed
                    (Done offsets)
                    |. chompUntilEndOr "\n"
                ]
    in
    loop [] helper
        |> Parser.map (List.map (Tuple.mapFirst inlineStyleBoundsToString))
        |> Parser.map Dict.fromList


inlineStyleOffsets : Parser ( InlineStyleBounds, InlineStyle )
inlineStyleOffsets =
    succeed
        (\bodyStart s bodyStop styleStart attrs styleStop ->
            ( InlineStyleBounds bodyStart bodyStop styleStart styleStop
            , Styled { styled = s, attrs = attrs }
            )
        )
        |= getOffset
        |. symbol "["
        |= (chompUntil "]"
                |> getChompedString
           )
        |. symbol "]"
        |= getOffset
        |. spaces
        |= getOffset
        |. symbol "{"
        |. spaces
        |. keyword "style"
        |. spaces
        |. symbol "|"
        |. spaces
        |= styleAttributes
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
        [ attribute Font "font" value
            |> backtrackable
        , attribute FontSize "size" int
            |> backtrackable
        , attribute Color "color" value
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
            (c /= ',')
                && (c /= '}')
        )
        |> getChompedString
        |> Parser.andThen
            (\s ->
                if s == "" then
                    problem "empty value"

                else
                    succeed (String.trimRight s)
            )



------------------------------------------------------------------------------
------------------------------------
-- Custom inlines positions index --
------------------------------------


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


chunks : Int -> List a -> List (List a)
chunks n xs =
    let
        helper acc ys =
            case ys of
                [] ->
                    List.reverse acc

                _ ->
                    helper (List.take n ys :: acc) (List.drop n ys)
    in
    helper [] xs


fonts =
    [ "Arial"
    , "Helvetica"
    , "Times New Roman"
    , "Times"
    , "Courier New"
    , "Courier"
    , "Verdana"
    , "Georgia"
    , "Palatino"
    , "Garamond"
    , "Bookman"
    , "Comic Sans MS"
    , "Trebuchet MS"
    , "Arial Black"
    , "Impact"
    , "Libre Baskerville"
    ]


fontSizes =
    [ "6"
    , "7"
    , "8"
    , "9"
    , "10"
    , "11"
    , "12"
    , "13"
    , "14"
    , "15"
    , "16"
    , "18"
    , "20"
    , "22"
    , "24"
    , "26"
    , "28"
    , "32"
    , "36"
    , "40"
    , "44"
    , "48"
    , "54"
    , "60"
    , "66"
    , "72"
    , "80"
    , "88"
    , "96"
    ]


buttonStyle isActive =
    [ Border.rounded 5
    , Font.center
    , centerY
    , padding 5
    , focused [ Border.glow (rgb 1 1 1) 0 ]
    ]
        ++ (if isActive then
                [ Background.color (rgb 0.9 0.9 0.9)
                , mouseOver [ Font.color (rgb 255 255 255) ]
                , Border.width 1
                , Border.color (rgb 0.9 0.9 0.9)
                ]

            else
                [ Background.color (rgb 0.95 0.95 0.95)
                , Font.color (rgb 0.7 0.7 0.7)
                , htmlAttribute <| HtmlAttr.style "cursor" "default"
                , Border.width 1
                , Border.color (rgb 0.95 0.95 0.95)
                ]
           )



-------------------------------------------------------------------------------
---------------------------------------
-- Color functions  and color picker --
---------------------------------------


colorPicker :
    String
    -> Bool
    -> Maybe String
    -> Msg
    -> (String -> Msg)
    -> String
    -> Element.Element Msg
colorPicker id colorPickerOpen currentColor openMsg handler label =
    let
        currentColor_ =
            currentColor
                |> Maybe.andThen (\c -> Dict.get c webColors)
                |> Maybe.map hexToColor
                |> Maybe.withDefault (rgb 1 1 1)

        colorPanView ( colname, colhex ) =
            el
                [ width (px 14)
                , height (px 14)
                , Background.color (hexToColor colhex)
                , Border.width 1
                , Border.color (rgb 0 0 0)
                , pointer
                , mouseOver
                    [ Border.color (rgb 0.9 0.9 0.9) ]
                , Events.onClick (handler colname)
                ]
                Element.none

        colors =
            chunks 12 (Dict.toList webColors)
                |> List.map
                    (\r ->
                        row [ spacing 3 ]
                            (List.map colorPanView r)
                    )
    in
    el
        [ below <|
            el
                [ Background.color (rgb 0.95 0.95 0.95) ]
                (if colorPickerOpen then
                    column
                        [ spacing 3
                        , padding 10
                        ]
                        colors

                 else
                    Element.none
                )
        , htmlAttribute <| HtmlAttr.id id
        ]
        (Input.button
            (buttonStyle True)
            { onPress = Just openMsg
            , label =
                row [ spacing 10 ]
                    [ el [] (text label)
                    , el
                        [ width (px 14)
                        , height (px 14)
                        , Background.color currentColor_
                        , Border.width 1
                        , Border.color (rgb 0 0 0)
                        ]
                        Element.none
                    ]
            }
        )


hexToColor : String -> Color
hexToColor hexColor =
    let
        hexColor_ =
            String.toLower hexColor

        red =
            String.left 2 hexColor_
                |> Hex.fromString
                |> Result.withDefault 0
                |> toFloat

        green =
            String.dropLeft 2 hexColor_
                |> String.left 2
                |> Hex.fromString
                |> Result.withDefault 0
                |> toFloat

        blue =
            String.dropLeft 4 hexColor_
                |> String.left 2
                |> Hex.fromString
                |> Result.withDefault 0
                |> toFloat
    in
    rgb (red / 255) (green / 255) (blue / 255)


webColors =
    Dict.fromList
        [ ( "maroon", "800000" )
        , ( "dark red", "8B0000" )
        , ( "brown", "A52A2A" )
        , ( "firebrick", "B22222" )
        , ( "crimson", "DC143C" )
        , ( "red", "FF0000" )
        , ( "tomato", "FF6347" )
        , ( "coral", "FF7F50" )
        , ( "indian red", "CD5C5C" )
        , ( "light coral", "F08080" )
        , ( "dark salmon", "E9967A" )
        , ( "salmon", "FA8072" )
        , ( "light salmon", "FFA07A" )
        , ( "orange red", "FF4500" )
        , ( "dark orange", "FF8C00" )
        , ( "orange", "FFA500" )
        , ( "gold", "FFD700" )
        , ( "dark golden rod", "B8860B" )
        , ( "golden rod", "DAA520" )
        , ( "pale golden rod", "EEE8AA" )
        , ( "dark khaki", "BDB76B" )
        , ( "khaki", "F0E68C" )
        , ( "olive", "808000" )
        , ( "yellow", "FFFF00" )
        , ( "yellow green", "9ACD32" )
        , ( "dark olive green", "556B2F" )
        , ( "olive drab", "6B8E23" )
        , ( "lawn green", "7CFC00" )
        , ( "chart reuse", "7FFF00" )
        , ( "green yellow", "ADFF2F" )
        , ( "dark green", "006400" )
        , ( "green", "008000" )
        , ( "forest green", "228B22" )
        , ( "lime", "00FF00" )
        , ( "lime green", "32CD32" )
        , ( "light green", "90EE90" )
        , ( "pale green", "98FB98" )
        , ( "dark sea green", "8FBC8F" )
        , ( "medium spring green", "00FA9A" )
        , ( "spring green", "0F0FF7F" )
        , ( "sea green", "2E8B57" )
        , ( "medium aqua marine", "66CDAA" )
        , ( "medium sea green", "3CB371" )
        , ( "light sea green", "20B2AA" )
        , ( "dark slate gray", "2F4F4F" )
        , ( "teal", "008080" )
        , ( "dark cyan", "008B8B" )
        , ( "aqua", "00FFFF" )
        , ( "cyan", "00FFFF" )
        , ( "light cyan", "E0FFFF" )
        , ( "dark turquoise", "00CED1" )
        , ( "turquoise", "40E0D0" )
        , ( "medium turquoise", "48D1CC" )
        , ( "pale turquoise", "AFEEEE" )
        , ( "aqua marine", "7FFFD4" )
        , ( "powder blue", "B0E0E6" )
        , ( "cadet blue", "5F9EA0" )
        , ( "steel blue", "4682B4" )
        , ( "corn flower blue", "6495ED" )
        , ( "deep sky blue", "00BFFF" )
        , ( "dodger blue", "1E90FF" )
        , ( "light blue", "ADD8E6" )
        , ( "sky blue", "87CEEB" )
        , ( "light sky blue", "87CEFA" )
        , ( "midnight blue", "191970" )
        , ( "navy", "000080" )
        , ( "dark blue", "00008B" )
        , ( "medium blue", "0000CD" )
        , ( "blue", "0000FF" )
        , ( "royal blue", "4169E1" )
        , ( "blue violet", "8A2BE2" )
        , ( "indigo", "4B0082" )
        , ( "dark slate blue", "483D8B" )
        , ( "slate blue", "6A5ACD" )
        , ( "medium slate blue", "7B68EE" )
        , ( "medium purple", "9370DB" )
        , ( "dark magenta", "8B008B" )
        , ( "dark violet", "9400D3" )
        , ( "dark orchid", "9932CC" )
        , ( "medium orchid", "BA55D3" )
        , ( "purple", "800080" )
        , ( "thistle", "D8BFD8" )
        , ( "plum", "DDA0DD" )
        , ( "violet", "EE82EE" )
        , ( "magenta / fuchsia", "FF00FF" )
        , ( "orchid", "DA70D6" )
        , ( "medium violet red", "C71585" )
        , ( "pale violet red", "DB7093" )
        , ( "deep pink", "FF1493" )
        , ( "hot pink", "FF69B4" )
        , ( "light pink", "FFB6C1" )
        , ( "pink", "FFC0CB" )
        , ( "antique white", "FAEBD7" )
        , ( "beige", "F5F5DC" )
        , ( "bisque", "FFE4C4" )
        , ( "blanched almond", "FFEBCD" )
        , ( "wheat", "F5DEB3" )
        , ( "corn silk", "FFF8DC" )
        , ( "lemon chiffon", "FFFACD" )
        , ( "light golden rod yellow", "FAFAD2" )
        , ( "light yellow", "FFFFE0" )
        , ( "saddle brown", "8B4513" )
        , ( "sienna", "A0522D" )
        , ( "chocolate", "D2691E" )
        , ( "peru", "CD853F" )
        , ( "sandy brown", "F4A460" )
        , ( "burly wood", "DEB887" )
        , ( "tan", "D2B48C" )
        , ( "rosy brown", "BC8F8F" )
        , ( "moccasin", "FFE4B5" )
        , ( "navajo white", "FFDEAD" )
        , ( "peach puff", "FFDAB9" )
        , ( "misty rose", "FFE4E1" )
        , ( "lavender blush", "FFF0F5" )
        , ( "linen", "FAF0E6" )
        , ( "old lace", "FDF5E6" )
        , ( "papaya whip", "FFEFD5" )
        , ( "sea shell", "FFF5EE" )
        , ( "mint cream", "F5FFFA" )
        , ( "slate gray", "708090" )
        , ( "light slate gray", "778899" )
        , ( "light steel blue", "B0C4DE" )
        , ( "lavender", "E6E6FA" )
        , ( "floral white", "FFFAF0" )
        , ( "alice blue", "F0F8FF" )
        , ( "ghost white", "F8F8FF" )
        , ( "honeydew", "F0FFF0" )
        , ( "ivory", "FFFFF0" )
        , ( "azure", "F0FFFF" )
        , ( "snow", "FFFAFA" )
        , ( "black", "000000" )
        , ( "dim gray / dim grey", "696969" )
        , ( "gray / grey", "808080" )
        , ( "dark gray / dark grey", "A9A9A9" )
        , ( "silver", "C0C0C0" )
        , ( "light gray / light grey", "D3D3D3" )
        , ( "gainsboro", "DCDCDC" )
        , ( "white smoke", "F5F5F5" )
        , ( "white", "FFFFFF" )
        ]



-------------------------------------------------------------------------------
---------------------------
-- Outside click decoder --
---------------------------


outsideTargetHandler : String -> msg -> D.Decoder msg
outsideTargetHandler targetId handler =
    D.field "target" (isOutsideTarget targetId)
        |> D.andThen
            (\isOutside ->
                if isOutside then
                    D.succeed handler

                else
                    D.fail "inside target"
            )


isOutsideTarget targetId =
    D.oneOf
        [ D.field "id" D.string
            |> D.andThen
                (\id ->
                    if targetId == id then
                        -- found match by id
                        D.succeed False

                    else
                        -- try next decoder
                        D.fail "continue"
                )
        , D.lazy (\_ -> D.field "parentNode" (isOutsideTarget targetId))

        -- fallback if all previous decoders failed
        , D.succeed True
        ]



-------------------------------------------------------------------------------


sampleString =
    """ Tarifs pour un séjour dans le gîte "Le vieux lilas" comprenant les prestations suivantes : mise à disposition de l'équipement inventorié, fourniture des draps, serviettes de toilette et linge de maison, ménage.

**La durée minimum du [ séjour ]{style| color: khaki} est de deux nuits.**

Les animaux de compagnie sont admis sous réserve qu'ils n'occasionnent aucunes dégradations ni nuisances sonores. Ils sont accueillis au gîte sans majoration tarifaire.

* 2 [ nuits ]{style| color: dodger blue} : [ 150 ]{style| color: crimson} €
* 3 nuits : [ 2 ]{style| color: aqua}[ 0 ]{style| color: dark orchid}0 €
* **1 [ semaine ]{style| color: cyan} : [ 350 ]{style| color: dark orchid} €**
* A partir de 4 nuits le tarif est calculé sur la base de 50 € la nuit

A ce tarif s'ajoute la taxe de séjour qui s'applique aux personnes majeures. Cliquez pour voir le document : [Tarif taxe de séjour Puisaye Forterre](https://gite-vieux-lilas.s3.eu-west-3.amazonaws.com/Documents/Affiche_Tarif_puisayeforterre_pourcentage_TA_web.pdf)

Des arrhes d'un montant de 30% seront à verser à la réservation.

Une caution de 50 € est demandée à l'arrivée dans le gîte. Elle sera remboursée à la fin du séjour sauf en cas de dégradation ou casse.

Pour réserver, choississez vos dates dans l'onglet "réservation" et indiquer vos coordonnées. Une confirmation vous sera envoyée par mail avec le contrat de location comprenant le descriptif détaillé.

Voir le [contrat_location_saisonnière_Le_vieux_lilas.pdf](https://gite-vieux-lilas.s3.eu-west-3.amazonaws.com/Documents/contrat_location_saisonnière_Le_vieux_lilas.pdf)
"""
