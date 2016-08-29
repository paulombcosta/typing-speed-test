module View exposing (..)

import Html exposing (div, Html, text, button, span)
import Html.Attributes exposing (style, class, id)
import Html.Events exposing (onClick)
import Array exposing (toList, Array)
import Types exposing (..)

import Material.Options as Options exposing (css, when)
import Material
import Material.Layout as Layout
import Material.Icon as Icon

-- scrolltop : changes the position in the view
-- getBoundClientRect : gets the boundaries of the view


view : Model -> Html Msg
view model = Layout.render Mdl model.mdl
    []
    { drawer = []
    , tabs = ([], [])
    , main = [ stylesheet model ]
    , header = header model
    }


stylesheet : Model -> Html Msg
stylesheet model =
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


header : Model -> List (Html Msg)
header model =
  [ Layout.row
      [ Options.nop
      , css "transition" "height 333ms ease-in-out 0s"
      ]
      [ Layout.title [] [ text "Typing Test" ]
      , Layout.spacer
      , Layout.navigation []
          [ Layout.link
              [ Layout.onClick NoOp ]
              [ Icon.i "photo" ]
          , Layout.link
              [ Layout.href "https://github.com/debois/elm-mdl"]
              [ span [] [text "github"] ]
          , Layout.link
              [ Layout.href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
              [ text "elm-package" ]
          ]
      ]
  ]
