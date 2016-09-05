module Bounds exposing (get, ClientRect)

import Native.Bounds
import String
import Maybe
import Task exposing (Task)


type alias ClientRect =
    { bottom : Float
    , height : Float
    , left : Float
    , right : Float
    , top : Float
    , width : Float
    }


get : String -> Task String (Maybe ClientRect)
get =
    Native.Bounds.get
