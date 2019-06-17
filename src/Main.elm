module Main exposing (Flags, Model, Msg(..), Path, doesSelectionStartsALine, findFirstOccIndex, indexLines, init, isSelectionALine, main, sampleString, subscriptions, update, view)

import Browser exposing (document)
import Browser.Events exposing (onAnimationFrame, onResize)
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


type alias Model =
    { seed : Random.Seed
    , maxWidth : Int
    , selected : Maybe Selection
    , setSelection : Maybe E.Value
    , rawInput : String
    , undoBuffer : List UndoAction
    , parsedInput : List (Block CustomBlock CustomInline)
    , stylesIndexes : Dict String CustomInline
    , selectionStyle : Maybe ( CustomInlineBounds, CustomInline )
    , articleStyle : List StyleAttribute
    , headingLevel : Int
    , openedWidget : Maybe Widget
    , setSelectionOnNextFrame : Bool
    }


type alias Path =
    String


type Widget
    = FontColorPicker
    | BackgroundColorPicker


type UndoAction
    = InputStringModif String
    | ArticleStyleModif (List StyleAttribute)


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
        , if model.setSelectionOnNextFrame then
            onAnimationFrame (\_ -> SetSelection)

          else
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
    | Undo
      --------------------
      -- Insert markdown--
      --------------------
    | InsertBold
    | InsertItalic
    | SelectHeadingLevel Int
    | InsertHeading
    | InsertList
      -----------------------
      -- Set custom styles --
      -----------------------
    | SetTextColor String
    | SetBackgroundColor String
    | SetFont String
    | SetFontSize Int
    | RemoveCustomStyle
      -----------
      -- Links --
      -----------
    | OpenInternalLinks
    | InsertInternalLink
    | OpenDocuments
    | InsertDocumentLink
    | InsertExternalLink
      ------------
      -- Medias --
      ------------
    | OpenImageWidget
    | PickImage
    | SetImageCaption
    | SetImageAlignment
    | InsertImage
    | InsertVideo
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { seed = Random.initialSeed flags.currentTime
      , maxWidth = flags.width
      , selected = Nothing
      , setSelection = Nothing
      , rawInput = sampleString
      , undoBuffer = []
      , parsedInput = []
      , stylesIndexes = Dict.empty
      , selectionStyle = Nothing
      , headingLevel = 1
      , articleStyle =
            [ Font "Times New Roman"
            , FontSize 18
            , Color "black"
            ]
      , openedWidget = Nothing
      , setSelectionOnNextFrame = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ---------------------------
        -- Textarea Manipulation --
        ---------------------------
        TextInput { selection, valueStr } ->
            let
                rawInput =
                    valueStr

                undoBuffer =
                    List.take 5 <| InputStringModif model.rawInput :: model.undoBuffer

                parsedInput =
                    Block.parse (Just { defaultOptions | softAsHardLineBreak = True })
                        valueStr
                        |> List.map addCustomInlines
                        |> List.concatMap addCustomBlocks

                stylesIndexes =
                    case Parser.run customStylesOffsets valueStr of
                        Ok indexes ->
                            indexes

                        _ ->
                            Dict.empty

                selectionStyle =
                    findCustomStyleFromCursorPos stylesIndexes selection
            in
            ( { model
                | rawInput = rawInput
                , undoBuffer = undoBuffer
                , parsedInput = parsedInput
                , stylesIndexes = stylesIndexes
                , selectionStyle = selectionStyle
                , setSelection = Nothing
              }
            , Cmd.none
            )

        NewSelection s ->
            let
                selectionStyle =
                    findCustomStyleFromCursorPos model.stylesIndexes s

                setSelection =
                    Maybe.map
                        (\( { styleStart, styleStop }, _ ) ->
                            encodeSelection styleStart styleStop
                        )
                        selectionStyle
            in
            ( { model
                | selected =
                    case selectionStyle of
                        Just ( { styleStart, styleStop }, _ ) ->
                            Just (Selection styleStart styleStop)

                        Nothing ->
                            Just s
                , selectionStyle = selectionStyle
                , setSelection =
                    Maybe.map
                        (\( { styleStart, styleStop }, _ ) ->
                            encodeSelection styleStart styleStop
                        )
                        selectionStyle
              }
            , Cmd.none
            )

        SetSelection ->
            ( { model
                | setSelection =
                    case ( model.selectionStyle, model.selected ) of
                        ( Just ( { styleStart, styleStop }, _ ), _ ) ->
                            Just <| encodeSelection styleStart styleStop

                        ( _, Just { start, stop } ) ->
                            Just <| encodeSelection start stop

                        _ ->
                            Nothing
                , setSelectionOnNextFrame = False
              }
            , Cmd.batch
                []
            )

        Undo ->
            case model.undoBuffer of
                [] ->
                    ( model, Cmd.none )

                (InputStringModif valueStr) :: xs ->
                    let
                        rawInput =
                            valueStr

                        undoBuffer =
                            xs

                        parsedInput =
                            Block.parse (Just { defaultOptions | softAsHardLineBreak = True })
                                valueStr
                                |> List.map addCustomInlines
                                |> List.concatMap addCustomBlocks

                        stylesIndexes =
                            case Parser.run customStylesOffsets valueStr of
                                Ok indexes ->
                                    indexes

                                _ ->
                                    Dict.empty

                        selected =
                            Nothing

                        selectionStyle =
                            Nothing
                    in
                    ( { model
                        | rawInput = rawInput
                        , undoBuffer = undoBuffer
                        , parsedInput = parsedInput
                        , stylesIndexes = stylesIndexes
                        , selected = selected
                        , selectionStyle = selectionStyle
                      }
                    , Cmd.none
                    )

                (ArticleStyleModif articleStyle) :: xs ->
                    ( { model
                        | articleStyle = articleStyle
                        , undoBuffer = xs
                      }
                    , Cmd.none
                    )

        ---------------------
        -- Insert markdown --
        ---------------------
        InsertBold ->
            ( insertMarkdown model insertBoldMarkdown
            , Cmd.none
            )

        InsertItalic ->
            ( insertMarkdown model insertItalicMarkdown
            , Cmd.none
            )

        SelectHeadingLevel level ->
            ( { model | headingLevel = level }, Cmd.none )

        InsertHeading ->
            ( insertMarkdown model (insertHeading model.rawInput model.selected model.headingLevel)
            , Cmd.none
            )

        InsertList ->
            ( model, Cmd.none )

        -----------------------
        -- Set custom styles --
        -----------------------
        SetTextColor color ->
            case model.selectionStyle of
                Nothing ->
                    ( insertCustomStyle { model | openedWidget = Nothing } [ Color color ]
                    , Cmd.none
                    )

                Just cs ->
                    ( updateCustomStyle { model | openedWidget = Nothing } cs [ Color color ]
                    , Cmd.none
                    )

        SetBackgroundColor color ->
            case model.selectionStyle of
                Nothing ->
                    ( insertCustomStyle { model | openedWidget = Nothing } [ BackgroundColor color ]
                    , Cmd.none
                    )

                Just cs ->
                    ( updateCustomStyle { model | openedWidget = Nothing } cs [ BackgroundColor color ]
                    , Cmd.none
                    )

        SetFont font ->
            case model.selectionStyle of
                Nothing ->
                    ( insertCustomStyle model [ Font font ]
                    , Cmd.none
                    )

                Just cs ->
                    ( updateCustomStyle model cs [ Font font ]
                    , Cmd.none
                    )

        SetFontSize n ->
            case model.selectionStyle of
                Nothing ->
                    ( insertCustomStyle model [ FontSize n ]
                    , Cmd.none
                    )

                Just cs ->
                    ( updateCustomStyle model cs [ FontSize n ]
                    , Cmd.none
                    )

        RemoveCustomStyle ->
            case model.selectionStyle of
                Nothing ->
                    ( model, Cmd.none )

                Just cs ->
                    ( removeCustomStyle model cs
                    , Cmd.none
                    )

        -----------
        -- Links --
        -----------
        OpenInternalLinks ->
            ( model, Cmd.none )

        InsertInternalLink ->
            ( model, Cmd.none )

        OpenDocuments ->
            ( model, Cmd.none )

        InsertDocumentLink ->
            ( model, Cmd.none )

        InsertExternalLink ->
            ( model, Cmd.none )

        ------------
        -- Medias --
        ------------
        OpenImageWidget ->
            ( model, Cmd.none )

        PickImage ->
            ( model, Cmd.none )

        SetImageCaption ->
            ( model, Cmd.none )

        SetImageAlignment ->
            ( model, Cmd.none )

        InsertImage ->
            ( model, Cmd.none )

        InsertVideo ->
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
                | openedWidget =
                    if model.openedWidget == Just FontColorPicker then
                        Nothing

                    else
                        Just FontColorPicker
              }
            , Cmd.none
            )

        OpenBackgroundColorPicker ->
            ( { model
                | openedWidget =
                    if model.openedWidget == Just BackgroundColorPicker then
                        Nothing

                    else
                        Just BackgroundColorPicker
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


findCustomStyleFromCursorPos : Dict String CustomInline -> Selection -> Maybe ( CustomInlineBounds, CustomInline )
findCustomStyleFromCursorPos stylesIndexes s =
    Dict.Extra.find
        (\k _ -> cursorInBounds s.start k)
        stylesIndexes
        |> Maybe.map (Tuple.mapFirst stringToCustomInlineBounds)
        |> Maybe.andThen
            (\( mbBounds, is ) ->
                case mbBounds of
                    Nothing ->
                        Nothing

                    Just b ->
                        Just ( b, is )
            )


insertMarkdown : Model -> (String -> String) -> Model
insertMarkdown model f =
    case model.selected of
        Just ({ start, stop } as sel) ->
            let
                prefix =
                    String.left start model.rawInput

                suffix =
                    String.dropLeft stop model.rawInput

                body =
                    f <| String.slice start stop model.rawInput

                newValueStr =
                    prefix ++ body ++ suffix

                selected =
                    Just <| Selection (String.length prefix) (String.length (prefix ++ body))
            in
            update (TextInput (CustomInput (Selection start (start + String.length newValueStr)) newValueStr))
                { model
                    | setSelectionOnNextFrame = True
                    , selected = selected
                }
                |> Tuple.first

        Nothing ->
            model


insertBoldMarkdown : String -> String
insertBoldMarkdown sel =
    let
        leftTrimmed =
            String.trimLeft sel

        trimmed =
            String.trimRight leftTrimmed

        leftSpace =
            String.left (String.length sel - String.length leftTrimmed) sel

        rightSpace =
            String.right (String.length leftTrimmed - String.length trimmed) sel
    in
    leftSpace ++ "**" ++ trimmed ++ "**" ++ rightSpace


insertItalicMarkdown : String -> String
insertItalicMarkdown sel =
    let
        leftTrimmed =
            String.trimLeft sel

        trimmed =
            String.trimRight leftTrimmed

        leftSpace =
            String.left (String.length sel - String.length leftTrimmed) sel

        rightSpace =
            String.right (String.length leftTrimmed - String.length trimmed) sel
    in
    leftSpace ++ "*" ++ trimmed ++ "*" ++ rightSpace


insertHeading : String -> Maybe Selection -> Int -> String -> String
insertHeading rawInput mbSelection level sel =
    if isSelectionALine rawInput mbSelection then
        nChar level '#' ++ " " ++ String.trimLeft sel

    else if doesSelectionStartsALine rawInput mbSelection then
        nChar level '#' ++ " " ++ String.trimLeft sel ++ "\n"

    else
        "\n" ++ nChar level '#' ++ " " ++ String.trimLeft sel ++ "\n"


nChar : Int -> Char -> String
nChar n c =
    if n == 0 then
        ""

    else
        String.cons c (nChar (n - 1) c)


indexLines : String -> Dict ( Int, Int ) String
indexLines input =
    String.lines input
        |> List.foldl
            (\l ( offset, index ) ->
                ( offset + 1 + String.length l
                , Dict.insert ( offset, offset + String.length l ) l index
                )
            )
            ( 0, Dict.empty )
        |> Tuple.second


isSelectionALine : String -> Maybe Selection -> Bool
isSelectionALine rawInput mbSelection =
    case mbSelection of
        Just { start, stop } ->
            Dict.member ( start, stop ) (indexLines rawInput)

        _ ->
            False


doesSelectionStartsALine : String -> Maybe Selection -> Bool
doesSelectionStartsALine rawInput mbSelection =
    case mbSelection of
        Just { start, stop } ->
            Dict.filter (\( x, _ ) _ -> x == start) (indexLines rawInput) /= Dict.empty

        _ ->
            False



-------------------------------------------------------------------------------


insertCustomStyle : Model -> List StyleAttribute -> Model
insertCustomStyle model newStyleAttrs =
    case model.selected of
        Just ({ start, stop } as sel) ->
            if start /= stop then
                let
                    newStyle =
                        Styled
                            { styled = String.slice start stop model.rawInput
                            , attrs = newStyleAttrs
                            }

                    newStyleStr =
                        customStyleToString newStyle

                    newSelection =
                        case Parser.run customStyleOffsets newStyleStr of
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
                update (TextInput (CustomInput newSelection newValueStr))
                    { model | setSelectionOnNextFrame = True }
                    |> Tuple.first

            else
                { model
                    | articleStyle =
                        uniqueBy styleAttrsCat <|
                            newStyleAttrs
                                ++ model.articleStyle
                    , undoBuffer =
                        List.take 5 <| ArticleStyleModif model.articleStyle :: model.undoBuffer
                }

        _ ->
            model


updateCustomStyle : Model -> ( CustomInlineBounds, CustomInline ) -> List StyleAttribute -> Model
updateCustomStyle model ( { styleStart, styleStop }, cs ) newStyleAttrs =
    let
        newStyle =
            combineCustomStyles cs newStyleAttrs

        newAttrStr =
            attrsToString newStyle

        prefix =
            String.left styleStart model.rawInput

        suffix =
            String.dropLeft styleStop model.rawInput

        newValueStr =
            prefix ++ newAttrStr ++ suffix
    in
    update (TextInput (CustomInput (Selection styleStart styleStop) newValueStr))
        { model | setSelectionOnNextFrame = True }
        |> Tuple.first


removeCustomStyle : Model -> ( CustomInlineBounds, CustomInline ) -> Model
removeCustomStyle model ( { bodyStart, bodyStop, styleStop }, _ ) =
    let
        prefix =
            String.left bodyStart model.rawInput

        body =
            String.slice (bodyStart + 1) (bodyStop - 1) model.rawInput

        suffix =
            String.dropLeft styleStop model.rawInput

        newValueStr =
            prefix ++ body ++ suffix
    in
    update (TextInput (CustomInput (Selection bodyStop bodyStop) newValueStr)) model
        |> Tuple.first


combineCustomStyles : CustomInline -> List StyleAttribute -> List StyleAttribute
combineCustomStyles current new =
    case current of
        Styled cs ->
            uniqueBy styleAttrsCat <| new ++ cs.attrs

        _ ->
            []



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


view : Model -> Browser.Document Msg
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
                [ --paragraph
                  --    [ Background.color <| rgba 0.8 0.5 0.7 0.3
                  --    , Font.family
                  --        [ Font.typeface "Times New Roman"
                  --        , Font.serif
                  --        ]
                  --    , Font.size 18
                  --    ]
                  --    [ text "Tarifs pour un séjour dans le gîte \" Le vieux lilas \" comprenant les prestations suivantes : mise à disposition de l'équipement inventorié, fourniture des draps, serviettes de toilette et linge de maison, ménage." ]
                  --,
                  markdownControlsView model
                , customStylesControlView model
                , customTextArea
                    [ width fill
                    , padding 15
                    ]
                    model.setSelection
                    500
                    model.rawInput
                , blocksToElements (\_ -> NoOp) model.parsedInput
                    |> (if model.maxWidth > 1000 then
                            textColumn

                        else
                            column
                       )
                        [ width fill
                        , spacing 15
                        , padding 15
                        , Border.rounded 5
                        , articleFont model.articleStyle
                        , articleFontSize model.articleStyle
                        , articleColor model.articleStyle
                        , articleBackgroundColor model.articleStyle
                        ]

                --, paragraph []
                --    [ text (Debug.toString model.parsedInput) ]
                --, text <| Debug.toString model.selectionStyle
                --, paragraph []
                --    [ text (Debug.toString <| Parser.run customStylesOffsets model.rawInput) ]
                ]
            )
        ]
    }


selectionInfoView : Model -> Element Msg
selectionInfoView model =
    let
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
        [ text <| "selection bounds: " ++ selectionView
        ]


markdownControlsView : Model -> Element Msg
markdownControlsView model =
    row
        [ spacing 15
        , paddingXY 15 0
        , Font.size 16
        ]
        [ Input.button
            (buttonStyle (canStyleSelection model))
            { onPress =
                if canStyleSelection model then
                    Just InsertBold

                else
                    Nothing
            , label = text "Bold"
            }
        , Input.button
            (buttonStyle (canStyleSelection model))
            { onPress =
                if canStyleSelection model then
                    Just InsertItalic

                else
                    Nothing
            , label = text "Italic"
            }
        , row
            [ spacing 15 ]
            [ el []
                (html <|
                    Html.select
                        [ HtmlEvents.onInput
                            (\level ->
                                String.toInt level
                                    |> Maybe.withDefault 1
                                    |> SelectHeadingLevel
                            )
                        ]
                        [ Html.option
                            [ HtmlAttr.value "1"
                            , HtmlAttr.selected (model.headingLevel == 1)
                            ]
                            [ Html.text "H1" ]
                        , Html.option
                            [ HtmlAttr.value "2"
                            , HtmlAttr.selected (model.headingLevel == 2)
                            ]
                            [ Html.text "H2" ]
                        , Html.option
                            [ HtmlAttr.value "3"
                            , HtmlAttr.selected (model.headingLevel == 3)
                            ]
                            [ Html.text "H3" ]
                        ]
                )
            , Input.button
                (buttonStyle (canStyleSelection model) ++ [ alignTop ])
                { onPress =
                    if canStyleSelection model then
                        Just InsertHeading

                    else
                        Nothing
                , label =
                    row [ spacing 5 ]
                        [ el [] (text "Insert heading")
                        ]
                }
            ]
        ]


customStylesControlView : Model -> Element Msg
customStylesControlView model =
    row
        [ spacing 15
        , paddingXY 15 0
        , Font.size 16
        ]
        [ colorPicker
            "fontColorPicker"
            (canCustomStyleSelection model)
            (model.openedWidget == Just FontColorPicker)
            (extractAttr extractColor model.articleStyle model.selectionStyle)
            OpenFontColorPicker
            SetTextColor
            "Set text color"
        , colorPicker
            "backgroundColorPicker"
            (canCustomStyleSelection model)
            (model.openedWidget == Just BackgroundColorPicker)
            (extractAttr extractBackgroundColor model.articleStyle model.selectionStyle)
            OpenBackgroundColorPicker
            SetBackgroundColor
            "Set background color"
        , fontsControllerView model
        , Input.button
            (buttonStyle (model.selectionStyle /= Nothing))
            { onPress =
                if model.selectionStyle /= Nothing then
                    Just RemoveCustomStyle

                else
                    Nothing
            , label = text "Remove Style"
            }
        , Input.button
            (buttonStyle (model.undoBuffer /= []))
            { onPress =
                if model.undoBuffer /= [] then
                    Just Undo

                else
                    Nothing
            , label = text "Undo"
            }
        ]


fontsControllerView : Model -> Element Msg
fontsControllerView model =
    let
        fontOptionView selectedFont f =
            Html.option
                [ HtmlAttr.value f
                , HtmlAttr.selected (selectedFont == (Just <| f))
                ]
                [ Html.text f ]

        fontSizeOptionView selectedSize fs =
            let
                selected =
                    String.toInt fs
                        |> Maybe.map (\fs_ -> selectedSize == (Just <| fs_))
                        |> Maybe.withDefault False
            in
            Html.option
                [ HtmlAttr.value fs
                , HtmlAttr.selected selected
                ]
                [ Html.text fs ]
    in
    row
        [ spacing 15 ]
        [ el
            []
            (html <|
                Html.select
                    [ HtmlEvents.onInput SetFont
                    , HtmlAttr.disabled (not <| canCustomStyleSelection model)
                    ]
                    (List.map
                        (fontOptionView
                            (extractAttr extractFont model.articleStyle model.selectionStyle)
                        )
                        (List.sort fonts)
                    )
            )
        , el
            []
            (html <|
                Html.select
                    [ HtmlEvents.onInput
                        (\n ->
                            String.toInt n
                                |> Maybe.withDefault 16
                                |> SetFontSize
                        )
                    , HtmlAttr.disabled (not <| canCustomStyleSelection model)
                    ]
                    (List.map
                        (fontSizeOptionView
                            (extractAttr extractFontSize model.articleStyle model.selectionStyle)
                        )
                        fontSizes
                    )
            )
        ]


canStyleSelection model =
    case model.selected of
        Nothing ->
            False

        Just { start, stop } ->
            case
                Dict.Extra.find
                    (\k _ -> selectionInBounds start stop k)
                    model.stylesIndexes
            of
                Just _ ->
                    False

                Nothing ->
                    True


canCustomStyleSelection model =
    case model.selected of
        Nothing ->
            False

        Just { start, stop } ->
            case
                Dict.Extra.find
                    (\k _ -> selectionInOrAroundBounds start stop k)
                    model.stylesIndexes
            of
                Just _ ->
                    model.selectionStyle /= Nothing

                Nothing ->
                    True


selectionInBounds start stop bounds =
    case stringToCustomInlineBounds bounds of
        Just { bodyStart, styleStop } ->
            ((start > bodyStart) && (start < styleStop)) || ((stop > bodyStart) && (stop < styleStop))

        _ ->
            False


selectionInOrAroundBounds start stop bounds =
    selectionInBounds start stop bounds
        || (case stringToCustomInlineBounds bounds of
                Just { bodyStart, styleStop } ->
                    (start <= bodyStart) && (stop >= styleStop)

                _ ->
                    False
           )



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
                    , HtmlAttr.style "width" "100%"
                    , HtmlAttr.style "height" (String.fromInt height ++ "px")
                    , HtmlAttr.style "padding" "15px"
                    , HtmlAttr.spellcheck False
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
            |> D.map NewSelection
        )



-------------------------------------------------------------------------------
-----------------------------------------
-- Custom markdown parser and renderer --
-----------------------------------------


blocksToElements : (String -> msg) -> List (Block CustomBlock CustomInline) -> List (Element msg)
blocksToElements downloadHandler blocks =
    List.map (blockToElement downloadHandler 0) blocks


blockToElement : (String -> msg) -> Int -> Block CustomBlock CustomInline -> Element msg
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
                [ width fill

                --, Background.color (rgba 0 1 1 0.2)
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
                                    []
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

        Block.Custom (ImageBlock { description, src, alignment }) _ ->
            column
                ([]
                    --Background.color (rgb 1 0 0) ]
                    ++ (case alignment of
                            AlignLeft ->
                                [ alignLeft
                                , paddingEach
                                    { sides
                                        | right = 10
                                    }
                                ]

                            AlignRight ->
                                [ alignRight
                                , paddingEach
                                    { sides | left = 10 }
                                ]

                            CenterAlign ->
                                [ centerX
                                ]
                       )
                )
                [ image
                    [ centerX ]
                    { src = src
                    , description = description
                    }
                , paragraph
                    [ Font.italic
                    , Font.size 14
                    , width fill
                    , Font.center
                    ]
                    [ text description ]
                ]


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


parseCustomInlines : Inline CustomInline -> List (Inline CustomInline)
parseCustomInlines inline =
    case inline of
        Text s ->
            case Parser.run customInlines s of
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

        Link url mbTitle inlines ->
            [ Link url mbTitle (List.concatMap parseCustomInlines inlines) ]

        Emphasis level inlines ->
            [ Emphasis level (List.concatMap parseCustomInlines inlines) ]

        _ ->
            [ inline ]


inlinesToElements : (String -> msg) -> List (Attribute msg) -> Inline CustomInline -> List (Element msg)
inlinesToElements downloadHandler attrs inline =
    case inline of
        Text s ->
            [ text s ]

        --el attrs (text s) ]
        HardLineBreak ->
            [ el attrs (html <| Html.br [] []) ]

        CodeInline s ->
            [ el attrs (text s) ]

        Link url mbTitle inlines ->
            if String.contains "Documents/" url then
                [ row
                    [ mouseOver
                        [ Font.color (rgb255 0 0 127)
                        ]
                    , Font.underline
                    , Font.color (rgb255 0 0 200)
                    , pointer
                    , Events.onClick (downloadHandler url)
                    ]
                    (List.concatMap (inlinesToElements downloadHandler attrs) inlines)
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
                           , width fill
                           ]
                    )
                    { url = url
                    , label =
                        --paragraph
                        --    []
                        --    (List.concatMap (inlinesToElements downloadHandler attrs) inlines)
                        text <| Inline.extractText inlines
                    }
                ]

        Image url mbTitle inlines ->
            [ image
                (attrs ++ [])
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

        Inline.Custom (CustomImage { description, src, alignment }) _ ->
            []

        Inline.Custom (Styled styled) inlines ->
            [ styledToElement attrs styled ]

        Inline.Custom (Regular _) _ ->
            []



-------------------------------------------------------------------------------
-------------------
-- Custom Blocks --
-------------------


type CustomBlock
    = ImageBlock
        { src : String
        , description : String
        , alignment : Alignment
        }



-------------------------------------------------------------------------------
-------------------------
-- Inline style parser --
-------------------------


type CustomInline
    = Styled
        { styled : String
        , attrs : List StyleAttribute
        }
    | CustomImage
        { src : String
        , description : String
        , alignment : Alignment
        }
    | Regular String


extractAttr : (StyleAttribute -> Maybe a) -> List StyleAttribute -> Maybe ( CustomInlineBounds, CustomInline ) -> Maybe a
extractAttr p articleStyle cs =
    case cs of
        Just ( _, Styled { attrs } ) ->
            case
                List.filterMap p attrs
                    |> List.head
            of
                Just customStyle_ ->
                    Just customStyle_

                Nothing ->
                    List.filterMap p articleStyle
                        |> List.head

        _ ->
            List.filterMap p articleStyle
                |> List.head


type StyleAttribute
    = Font String
    | FontSize Int
    | Color String
    | BackgroundColor String



--| Align Alignment


type Alignment
    = AlignLeft
    | AlignRight
    | CenterAlign


styleAttrsCat : StyleAttribute -> String
styleAttrsCat sa =
    case sa of
        Font _ ->
            "Font"

        FontSize _ ->
            "FontSize"

        Color _ ->
            "Color"

        BackgroundColor _ ->
            "BackgroundColor"



--Align _ ->
--    "alignment"


articleFont : List StyleAttribute -> Attribute msg
articleFont attrs =
    List.filterMap extractFont attrs
        |> List.head
        |> Maybe.withDefault "Times New Roman"
        |> (\f ->
                Font.family
                    [ Font.typeface f ]
           )


extractFont : StyleAttribute -> Maybe String
extractFont attr =
    case attr of
        Font f ->
            Just f

        _ ->
            Nothing


articleFontSize : List StyleAttribute -> Attribute msg
articleFontSize attrs =
    List.filterMap extractFontSize attrs
        |> List.head
        |> Maybe.withDefault 18
        |> (\fs -> Font.size fs)


extractFontSize : StyleAttribute -> Maybe Int
extractFontSize attr =
    case attr of
        FontSize n ->
            Just n

        _ ->
            Nothing


articleColor : List StyleAttribute -> Attribute msg
articleColor attrs =
    List.filterMap extractColor attrs
        |> List.head
        |> Maybe.andThen (\s -> Dict.get s webColors)
        |> Maybe.withDefault "000000"
        |> hexToColor
        |> Font.color


extractColor : StyleAttribute -> Maybe String
extractColor attr =
    case attr of
        Color c ->
            Just c

        _ ->
            Nothing


articleBackgroundColor : List StyleAttribute -> Attribute msg
articleBackgroundColor attrs =
    List.filterMap extractBackgroundColor attrs
        |> List.head
        |> Maybe.andThen (\s -> Dict.get s webColors)
        |> Maybe.withDefault "FFFFFF"
        |> hexToColor
        |> Background.color


extractBackgroundColor : StyleAttribute -> Maybe String
extractBackgroundColor attr =
    case attr of
        BackgroundColor c ->
            Just c

        _ ->
            Nothing



-------------------------------------------------------------------------------


customStyleToString : CustomInline -> String
customStyleToString is =
    case is of
        Styled { styled, attrs } ->
            "[" ++ styled ++ "]" ++ attrsToString attrs

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

                BackgroundColor c ->
                    "background color: " ++ c

        --Align a ->
        --    "align: " ++ alignmentToStr a
    in
    List.map attrToStr attrs
        |> String.join ", "
        |> (\res -> "{style| " ++ res ++ "}")


alignmentToStr a =
    case a of
        AlignLeft ->
            "left"

        AlignRight ->
            "right"

        CenterAlign ->
            "center"


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

        BackgroundColor s ->
            attrs
                ++ [ Dict.get s webColors
                        |> Maybe.withDefault "000000"
                        |> hexToColor
                        |> Background.color
                   ]


customInlines : Parser (List CustomInline)
customInlines =
    let
        helper styles =
            oneOf
                [ succeed (\style -> Loop (style :: styles))
                    |= customStyle
                    |> backtrackable
                , succeed (\style -> Loop (style :: styles))
                    |= customImage
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

                (Regular r) :: xs_ ->
                    concatRegs (acc ++ r) res xs_

                other :: xs_ ->
                    if acc == "" then
                        concatRegs "" (other :: res) xs_

                    else
                        concatRegs "" (other :: Regular acc :: res) xs_
    in
    loop [] helper


type alias CustomInlineBounds =
    { bodyStart : Int
    , bodyStop : Int
    , styleStart : Int
    , styleStop : Int
    }


customStyleBoundsToString : CustomInlineBounds -> String
customStyleBoundsToString { bodyStart, bodyStop, styleStart, styleStop } =
    String.fromInt bodyStart
        ++ "-"
        ++ String.fromInt bodyStop
        ++ "-"
        ++ String.fromInt styleStart
        ++ "-"
        ++ String.fromInt styleStop


cursorInBounds : Int -> String -> Bool
cursorInBounds cursorPos bounds =
    case stringToCustomInlineBounds bounds of
        Just { styleStart, styleStop } ->
            (cursorPos >= styleStart) && (cursorPos < styleStop)

        _ ->
            False


stringToCustomInlineBounds : String -> Maybe CustomInlineBounds
stringToCustomInlineBounds s =
    case
        String.split "-" s
            |> List.filterMap String.toInt
    of
        bodyStart :: bodyStop :: styleStart :: styleStop :: [] ->
            Just <| CustomInlineBounds bodyStart bodyStop styleStart styleStop

        _ ->
            Nothing


customStylesOffsets : Parser (Dict String CustomInline)
customStylesOffsets =
    let
        helper offsets =
            oneOf
                [ succeed (\offset -> Loop (offset :: offsets))
                    |= customStyleOffsets
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
        |> Parser.map (List.map (Tuple.mapFirst customStyleBoundsToString))
        |> Parser.map Dict.fromList


customStyleOffsets : Parser ( CustomInlineBounds, CustomInline )
customStyleOffsets =
    succeed
        (\bodyStart s bodyStop styleStart attrs styleStop ->
            ( CustomInlineBounds bodyStart bodyStop styleStart styleStop
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


customStyle : Parser CustomInline
customStyle =
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


customImage : Parser CustomInline
customImage =
    succeed
        (\dscr src align ->
            CustomImage
                { src = src
                , description = dscr
                , alignment = align
                }
        )
        |. symbol "["
        |= (chompUntil "]"
                |> getChompedString
           )
        |. symbol "]"
        |. spaces
        |. symbol "{"
        |. spaces
        |. keyword "image"
        |. spaces
        |. symbol "|"
        |. spaces
        |= attribute identity "src" value
        |. spaces
        |. symbol ","
        |. spaces
        |= attribute identity "align" alignmentParser
        |. spaces
        |. symbol "}"


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
            |> backtrackable

        --, attribute Align "align" alignment
        --    |> backtrackable
        , attribute BackgroundColor "background color" value
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
        |> Parser.map String.trimRight
        |> Parser.andThen
            (\s ->
                if s == "" then
                    problem "empty value"

                else
                    succeed s
            )


alignmentParser : Parser Alignment
alignmentParser =
    value
        |> Parser.andThen
            (\a ->
                if a == "left" then
                    succeed AlignLeft

                else if a == "right" then
                    succeed AlignRight

                else if a == "center" then
                    succeed CenterAlign

                else
                    problem "invalid aligment"
            )



------------------------------------------------------------------------------
------------------------------------
-- Custom inlines positions index --
------------------------------------


addCustomInlines : Block CustomBlock CustomInline -> Block CustomBlock CustomInline
addCustomInlines block =
    case block of
        BlankLine s ->
            BlankLine s

        ThematicBreak ->
            ThematicBreak

        Heading raw level inlines ->
            Heading raw level (List.concatMap parseCustomInlines inlines)

        CodeBlock cb raw ->
            CodeBlock cb raw

        Paragraph raw inlines ->
            Paragraph raw (List.concatMap parseCustomInlines inlines)

        BlockQuote blocks ->
            BlockQuote (List.map addCustomInlines blocks)

        List listblock llistBlocks ->
            List listblock (List.map (List.map addCustomInlines) llistBlocks)

        PlainInlines inlines ->
            PlainInlines (List.concatMap parseCustomInlines inlines)

        Block.Custom b blocks ->
            Block.Custom b (List.map addCustomInlines blocks)


addCustomBlocks : Block CustomBlock CustomInline -> List (Block CustomBlock CustomInline)
addCustomBlocks block =
    case block of
        Paragraph raw inlines ->
            extractCustomInlines inlines

        other ->
            [ other ]


extractCustomInlines : List (Inline CustomInline) -> List (Block CustomBlock CustomInline)
extractCustomInlines inlines =
    let
        helper blocksAcc inlinesAcc rest =
            case rest of
                x :: xs ->
                    case inlineToCustomBlock x of
                        Just b ->
                            helper
                                (b
                                    :: (if inlinesAcc == [] then
                                            []

                                        else
                                            let
                                                is =
                                                    List.reverse inlinesAcc
                                            in
                                            [ Paragraph (Inline.extractText is) is ]
                                       )
                                    ++ blocksAcc
                                )
                                []
                                xs

                        Nothing ->
                            helper
                                blocksAcc
                                (x :: inlinesAcc)
                                xs

                [] ->
                    List.reverse <|
                        (if inlinesAcc == [] then
                            []

                         else
                            let
                                is =
                                    List.reverse inlinesAcc
                            in
                            [ Paragraph (Inline.extractText is) is ]
                        )
                            ++ blocksAcc
    in
    helper [] [] inlines


inlineToCustomBlock : Inline CustomInline -> Maybe (Block CustomBlock CustomInline)
inlineToCustomBlock inline =
    case inline of
        Inline.Custom (CustomImage meta) _ ->
            Just <| Block.Custom (ImageBlock meta) []

        _ ->
            Nothing



-------------------------------------------------------------------------------
----------
-- Misc --
----------


sides =
    { top = 0
    , left = 0
    , right = 0
    , bottom = 0
    }


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
    -> Bool
    -> Maybe String
    -> Msg
    -> (String -> Msg)
    -> String
    -> Element.Element Msg
colorPicker id isActive colorPickerOpen currentColor openMsg handler label =
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
            (buttonStyle isActive)
            { onPress =
                if isActive then
                    Just openMsg

                else
                    Nothing
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
    """Tarifs pour un séjour dans le gîte "Le vieux lilas" comprenant les prestations suivantes : mise à disposition de l'équipement inventorié, fourniture des draps, serviettes de toilette et linge de maison, ménage.

**La durée minimum du [ séjour ]{style| color: khaki} est de deux nuits.**

[elm logo]{image| src: https://s14-eu5.startpage.com/wikioimage/5c5cdc254ff34ff9d620f47cb88c3d0b.png, align: left} Les animaux de compagnie sont admis sous réserve qu'ils n'occasionnent aucunes dégradations ni nuisances sonores. Ils sont accueillis au gîte sans majoration tarifaire.

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


sampleString2 =
    """Hello!
How do you do?"""
