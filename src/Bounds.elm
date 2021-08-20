port module Bounds exposing (BoundingClientRect, fetchBoundingClientRect, origin, setBoundingClientRect)

import Browser.Dom exposing (Error, Viewport, getViewportOf)
import Task exposing (Task)


type alias BoundingClientRect =
    { left : Float, top : Float, right : Float, bottom : Float, x : Float, y : Float, width : Float, height : Float }


origin : BoundingClientRect
origin =
    { left = 0, top = 0, right = 0, bottom = 0, x = 0, y = 0, width = 0, height = 0 }


port fetchBoundingClientRect : String -> Cmd msg


port setBoundingClientRect : (Maybe BoundingClientRect -> msg) -> Sub msg
