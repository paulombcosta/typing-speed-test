module App exposing (..)

import State
import View
import Html


main =
    Html.program
        { init = State.initialState
        , view = View.view
        , update = State.update
        , subscriptions = State.subscriptions
        }
