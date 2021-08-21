module View exposing (..)

import Array exposing (Array, fromList, toList)
import Html exposing (Attribute, Html, a, button, div, p, span, text)
import Html.Attributes exposing (class, classList, id, style)
import Html.Events exposing (onClick)
import String exposing (fromChar, fromInt)
import Types exposing (..)


view : Model -> Html Msg
view model =
    stylesheet model


stylesheet : Model -> Html Msg
stylesheet model =
    div
        [ class "root" ]
        [ header
        , stats model
        , wordsBox model
        , currentTypedChars model
        ]


stats : Model -> Html Msg
stats model =
    div [ class "stats-container" ]
        [ div [ class "status" ] [ p [ class "status-text" ] [ text (statusText model) ] ]
        , div [ class "metrics" ]
            [ div [] [ p [ class "wpm" ] [ text ("WPM " ++ getWPM model) ] ]
            , div [] [ p [ class "cpm" ] [ text ("CPM " ++ getCPM model) ] ]
            ]
        ]


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


statusText : Model -> String
statusText model =
    case model.applicationStatus of
        NotStarted ->
            "Start typing to start the test"

        Started ->
            timeLeft model

        Finished ->
            "You're done, press restart to try again!"


currentTypedChars : Model -> Html Msg
currentTypedChars model =
    div [ class "current-typed-chars" ] [ text (getCurrentTypedWords model) ]


getCurrentTypedWords : Model -> String
getCurrentTypedWords model =
    case model.applicationStatus of
        NotStarted ->
            "The word you are typing will appear here"

        _ ->
            arrayToString model.currentTypedChars


header : Html Msg
header =
    div
        [ class "header" ]
        [ p [ class "header-text" ] [ text "Typing Speed Test" ]
        , a
            [ classList [ ( "restart", True ), ( "header-text", True ) ], onClick Restart ]
            [ text "restart" ]
        ]


wordsBox : Model -> Html Msg
wordsBox model =
    div
        [ class "typing", id "typing" ]
        [ div [ class "words-box" ] (wordsToHTML model) ]


testScrollComponent : Html Msg
testScrollComponent =
    div [] [ button [ onClick TestScroll ] [ text "Test scroll" ] ]


timeLeft : Model -> String
timeLeft model =
    model.timeLimitSeconds
        - model.timePassedSeconds
        |> fromInt


wpm : Model -> String
wpm model =
    model.currentWords
        |> Array.filter (\w -> w.wordStatus == TypedCorrectly)
        |> Array.length
        |> toFloat
        |> (\x -> approximateWPM x model)
        |> sanitize
        |> round
        |> fromInt


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


cpm : Model -> String
cpm model =
    model.currentWords
        |> Array.filter (\w -> w.wordStatus == TypedCorrectly)
        |> Array.map (\w -> String.length w.typedText)
        |> Array.foldl (+) 0
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


wordsToHTML : Model -> List (Html Msg)
wordsToHTML model =
    let
        words =
            model.currentWords

        currentPosition =
            model.currentPosition
    in
    words
        |> Array.indexedMap
            (\idx word ->
                if idx == currentPosition then
                    div
                        [ class "currentWord"
                        , id ("word-" ++ fromInt idx)
                        ]
                        (currentWordProgress model.currentTypedChars word)

                else
                    div
                        [ getWordStyle word
                        , class "word"
                        , id ("word-" ++ fromInt idx)
                        ]
                        [ text word.text ]
            )
        |> toList


currentWordProgress : Array String -> Word -> List (Html Msg)
currentWordProgress currentTypedWords word =
    let
        wordTextAsList =
            word.text |> String.toList |> List.map (\x -> fromChar x)

        currentWordArray =
            fromList wordTextAsList
    in
    currentWordArray
        |> Array.indexedMap
            (\idx char ->
                spanForCurrentWord (Array.get idx currentTypedWords) char
            )
        |> toList


spanForCurrentWord : Maybe String -> String -> Html Msg
spanForCurrentWord typedChar expectedChar =
    case typedChar of
        Nothing ->
            span [] [ text expectedChar ]

        Just c ->
            if expectedChar == c then
                span [ style "color" "#7FFF00" ] [ text expectedChar ]

            else
                span [ style "color" "red" ] [ text expectedChar ]


getWordStyle : Word -> Attribute Msg
getWordStyle word =
    case word.wordStatus of
        Unevaluated ->
            style "color" "black"

        TypedCorrectly ->
            style "color" "#7FFF00"

        TypedIncorrectly ->
            style "color" "red"


arrayToString : Array String -> String
arrayToString array =
    Array.foldr (++) "" array
