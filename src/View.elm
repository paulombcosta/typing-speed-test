module View exposing (..)

import Html exposing (div, Html, text, button, span, p)
import Html.Attributes exposing (style, class, id)
import Html.Events exposing (onClick)
import Array exposing (toList, Array, fromList)
import Types exposing (..)
import String exposing (fromChar)


view : Model -> Html Msg
view model = stylesheet model


stylesheet : Model -> Html Msg
stylesheet model =
    div
        [ class "root" ]
        [ header
        , wordsBox model
        , currentTypedChars model
        ]


--stateSelector : Model -> List (Html.Html Msg)
--stateSelector model =
--   case model.applicationStatus of
--       Started -> startedApplicationBody model
--       NotStarted -> notStartedApplicationBody model
--       Finished -> finishedApplicationBody model

--notStartedApplicationBody : Model -> List (Html.Html Msg)
--notStartedApplicationBody model =
--    [ div [] [text "Not started"]
--    , button [ onClick StartApp] [text "Start"]
--    ]

currentTypedChars : Model -> Html.Html Msg
currentTypedChars model =
    div [ class "current-typed-chars"] [ text (arrayToString model.currentTypedChars) ]


header : Html.Html Msg
header =
    div
        [ class "header" ]
        [ p [ class "header-text" ] [text "Typing Speed Test" ]]


wordsBox : Model -> Html.Html Msg
wordsBox model =
     div
        [ class "typing", id "typing" ]
        [ div [] (wordsToHTML (model))]


testScrollComponent : Html.Html Msg
testScrollComponent = div [] [ button [ onClick TestScroll ] [ text "Test scroll" ] ]

countdown: Model -> Html.Html Msg
countdown model = div [] [ p [] [ text (timeLeft model) ] ]

timeLeft : Model -> String
timeLeft model =
    model.timeLimitSeconds - model.timePassedSeconds
    |> toString

--finishedApplicationBody : Model -> List (Html.Html Msg)
--finishedApplicationBody model =
--    [ div [] [text "We're finished here!"]
--    , p [] [text ("You WPM is " ++ (wpm model)) ]
--    , p [] [text ("You CPM is " ++ (cpm model)) ]
--    ]

-- This assumes that the time of the test was exactly one minute
wpm : Model -> String
wpm model =
    model.currentWords
    |> Array.filter (\w -> w.wordStatus == TypedCorrectly)
    |> Array.length
    |> toFloat
    |> round
    |> toString


-- This assumes that the time of the test was exactly one minute
cpm : Model -> String
cpm model =
    model.currentWords
    |> Array.filter (\w -> w.wordStatus == TypedCorrectly)
    |> Array.map (\w -> String.length w.typedText)
    |> Array.foldl (+) 0
    |> toString


divideBy60 : Float -> Float
divideBy60 x = x / 60

wordsToHTML : Model -> List (Html.Html Msg)
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
                    if (idx == currentPosition) then
                        div
                            [ class "currentWord"
                            , id ("word-" ++ (toString idx))
                            ]
                            (currentWordProgress model.currentTypedChars word)
                    else
                        div
                            [ style [ getWordStyle word ]
                            , class "word"
                            , id ("word-" ++ (toString idx))
                            ]
                            [ text word.text ]
                )
            |> toList


currentWordProgress : Array String -> Word -> List (Html.Html Msg)
currentWordProgress currentTypedWords word =
    let
        wordTextAsList =
            word.text |> String.toList |> List.map (\x -> fromChar x)

        currentWordArray =
            fromList (wordTextAsList)
    in
        currentWordArray
            |> Array.indexedMap
                (\idx char ->
                    spanForCurrentWord (Array.get idx currentTypedWords) char
                )
            |> toList


spanForCurrentWord : Maybe String -> String -> Html.Html Msg
spanForCurrentWord typedChar expectedChar =
    case (typedChar) of
        Nothing ->
            span [] [ text expectedChar ]

        Just c ->
            if (expectedChar == c) then
                span [ style [ ( "color", "#7FFF00" ) ] ] [ text expectedChar ]
            else
                span [ style [ ( "color", "red" ) ] ] [ text expectedChar ]


getWordStyle : Word -> ( String, String )
getWordStyle word =
    case word.wordStatus of
        Unevaluated ->
            ( "color", "black" )

        TypedCorrectly ->
            ( "color", "#7FFF00" )

        TypedIncorrectly ->
            ( "color", "red" )


arrayToString : Array String -> String
arrayToString array =
    Array.foldr (++) "" array
