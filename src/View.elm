module View exposing (..)

import Array exposing (Array, fromList, toList)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (class)
import String exposing (fromChar, fromInt)
import Types exposing (..)


view : Model -> Html Msg
view model =
    stylesheet model


stylesheet : Model -> Html Msg
stylesheet model =
    layout [ Background.color colors.darkBg ] <|
        column [ width fill, height (px 800) ]
            [ countdown model
            , wordsCanvas model
            , restartIcon
            , stats model
            ]


countdown : Model -> Element Msg
countdown model =
    case model.applicationStatus of
        Started ->
            el [ centerX, centerY, Font.color colors.mainColor ] <| Element.text <| timeLeft model

        _ ->
            el [ centerX, centerY, Font.color colors.mainColor ] <| Element.text <| String.fromInt model.timeLimitSeconds


stats : Model -> Element Msg
stats model =
    if model.applicationStatus == Finished then
        column [ width fill, spacing 30 ]
            [ el [ centerX, Font.size 24, Font.color colors.wordCorrect ] <| Element.text <| statusText model
            , row [ centerX, spacing 30 ]
                [ row [] [ el [ Font.size 24, Font.color colors.wordDefault ] <| Element.text "WPM: ", el [ Font.size 24, Font.color colors.mainColor ] <| Element.text <| getWPM model ]
                , row [] [ el [ Font.size 24, Font.color colors.wordDefault ] <| Element.text "CPM: ", el [ Font.size 24, Font.color colors.mainColor ] <| Element.text <| getCPM model ]
                ]
            ]

    else
        Element.none


statusText : Model -> String
statusText model =
    case model.applicationStatus of
        Finished ->
            "Test finished. Your stats: "

        _ ->
            ""


timeLeft : Model -> String
timeLeft model =
    model.timeLimitSeconds
        - model.timePassedSeconds
        |> fromInt


getWPM : Model -> String
getWPM model =
    case model.applicationStatus of
        NotStarted ->
            "0"

        _ ->
            wpm model


getCPM : Model -> String
getCPM model =
    case model.applicationStatus of
        NotStarted ->
            "0"

        _ ->
            cpm model


cpm : Model -> String
cpm model =
    model.evaluatedWords
        |> List.filter (\w -> w.wordStatus == TypedCorrectly)
        |> List.map (\w -> String.length w.typedText)
        |> List.foldl (+) 0
        |> (\x -> approximateCPM (toFloat x) model)
        |> sanitize
        |> round
        |> fromInt


approximateCPM : Float -> Model -> Float
approximateCPM x model =
    if x <= 0 then
        0

    else
        x * 60 / toFloat model.timePassedSeconds


approximateWPM : Float -> Model -> Float
approximateWPM x model =
    if x <= 0 then
        0

    else
        x * 60 / toFloat model.timePassedSeconds


sanitize : Float -> Float
sanitize x =
    if isInfinite x then
        0

    else
        x


wpm : Model -> String
wpm model =
    model.evaluatedWords
        |> List.filter (\w -> w.wordStatus == TypedCorrectly)
        |> List.length
        |> toFloat
        |> (\x -> approximateWPM x model)
        |> sanitize
        |> round
        |> fromInt


wordsCanvas : Model -> Element Msg
wordsCanvas model =
    column [ centerX, centerY ] <| rows model


rows : Model -> List (Element Msg)
rows model =
    Dict.toList model.rows
        |> List.map (\x -> wordsRow (Tuple.first x) model (Tuple.second x))


wordsRow : Int -> Model -> Array Word -> Element Msg
wordsRow rowIndex model words =
    words
        |> Array.indexedMap (\idx w -> wordToElem w model rowIndex idx)
        |> toList
        |> row [ Font.size 24 ]


wordToElem : Word -> Model -> Int -> Int -> Element Msg
wordToElem word model rowIndex wordIndex =
    let
        currentWordIndex =
            model.currentWordIndex

        currentRowIdx =
            model.currentRowIndex
    in
    -- The current word being typed right now
    if rowIndex == currentRowIdx && wordIndex == currentWordIndex then
        currentWordProgress model.currentTypedChars word

    else
        el [ Font.color (getWordColor word), padding 7 ] <| text word.text


getWordColor : Word -> Color
getWordColor w =
    case w.wordStatus of
        Unevaluated ->
            colors.wordDefault

        TypedCorrectly ->
            colors.wordCorrect

        TypedIncorrectly ->
            colors.wordErr


currentWordProgress : Array String -> Word -> Element Msg
currentWordProgress currentTypedWords word =
    let
        wordTextAsList =
            word.text |> String.toList |> List.map fromChar

        elems =
            wordTextAsList
                |> fromList
                |> Array.indexedMap (\idx char -> elemForCurrentWord (Array.get idx currentTypedWords) char)
                |> attachCursor currentTypedWords
                |> toList
    in
    row
        [ padding 7 ]
        elems


attachCursor : Array String -> Array (Element Msg) -> Array (Element Msg)
attachCursor typedWords elems =
    let
        lenTyped =
            Array.length typedWords

        lenTotal =
            Array.length elems
    in
    if lenTyped == 0 then
        Array.append (Array.fromList [ cursor ]) elems

    else if lenTyped == Array.length elems then
        Array.push cursor elems

    else
        Array.slice 0 lenTyped elems
            |> Array.push cursor
            |> (\a -> Array.append a (Array.slice lenTyped (lenTotal + 1) elems))


cursor : Element Msg
cursor =
    el [ height (px 30), width (px 2), Background.color colors.mainColor ] <| Element.none


elemForCurrentWord : Maybe String -> String -> Element Msg
elemForCurrentWord typedChar expectedChar =
    case typedChar of
        Nothing ->
            el [ Font.color colors.wordDefault ] <| Element.text expectedChar

        Just c ->
            if expectedChar == c then
                el [ Font.color colors.wordCorrect ] <| Element.text expectedChar

            else
                el [ Font.color colors.wordErr ] <| Element.text expectedChar


currentTypedChars : Model -> Element Msg
currentTypedChars model =
    el [ centerX, centerY ] <| Element.text (getCurrentTypedWords model)


getCurrentTypedWords : Model -> String
getCurrentTypedWords model =
    case model.applicationStatus of
        NotStarted ->
            "The word you are typing will appear here"

        _ ->
            model.currentTypedChars
                |> Array.foldr (++) ""


restartIcon : Element Msg
restartIcon =
    Html.i [ class "material-icons" ] [ Html.text "replay" ]
        |> Element.html
        |> el [ Events.onClick Restart, Element.pointer, centerX, centerY, Font.color colors.wordDefault, paddingXY 0 20 ]
