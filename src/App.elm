module App exposing (..)

import State
import View
import Html


main =
    Html.program
        { init = State.initalState
        , view = View.view
        , update = State.update
        , subscriptions = State.subscriptions
        }
