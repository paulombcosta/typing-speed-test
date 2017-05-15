module Bounds exposing (get, ClientRect, origin)

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


origin : ClientRect
origin = {bottom = 0, height = 0, left = 0, right = 0, top = 0, width = 0}

get : String -> Task String (Maybe ClientRect)
get =
    Native.Bounds.get
