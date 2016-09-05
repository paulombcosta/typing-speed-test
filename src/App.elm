module App exposing (..)

import State
import View
import Html.App


main =
    Html.App.program
        { init = State.initalState
        , view = View.view
        , update = State.update
        , subscriptions = State.subscriptions
        }
