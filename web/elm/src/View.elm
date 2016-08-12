module View exposing (..)

import Html exposing (div, Html, text, button)
import Html.Attributes exposing (style, class, id)
import Html.Events exposing (onClick)
import Array exposing (toList, Array)
import Types exposing (..)

-- scrolltop : changes the position in the view
-- getBoundClientRect : gets the boundaries of the view



view : Model -> Html Msg
view model =
  div [ class "root"]
      [ div
          [ class "typing"]
          [
              div [] (wordsToHTML (model.currentWords))
            , div [] [ (text (arrayToString model.currentTypedChars)) ]
          ]
      ]

wordsToHTML : Array Word -> List (Html.Html Msg)
wordsToHTML words =
    words |> Array.indexedMap (\idx word -> div
    [
        style [ getWordStyle word ]
      , class "word"
      , id ("word-" ++ (toString idx))
    ]
    [ text word.text ])
    |> toList


getWordStyle : Word -> (String, String)
getWordStyle word =
    case word.wordStatus of
        Unevaluated ->
            ("color", "black")
        TypedCorrectly ->
            ("color", "#7FFF00")
        TypedIncorrectly ->
            ("color", "red")


arrayToString : Array String -> String
arrayToString array =
    Array.foldr (++) "" array

