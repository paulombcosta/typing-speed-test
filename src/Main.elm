module Main exposing (..)

import Browser
import State
import Types exposing (Model, Msg)
import View


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> State.initialState
        , view = View.view
        , update = State.update
        , subscriptions = State.subscriptions
        }
