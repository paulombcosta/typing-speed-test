module Types exposing (..)

import Array exposing (Array)
import Time exposing (Time)
import Keyboard
import Material

type alias Word =
    { text : String
    , typedText : String
    , wordStatus : WordStatus
    }

type Msg
    = NoOp
    | TimeForInitialSeed Time
    | KeyTyped Keyboard.KeyCode
    | Mdl (Material.Msg Msg)


type ApplicationStatus
    = Started
    | Finished


type WordStatus
    = Unevaluated
    | TypedCorrectly
    | TypedIncorrectly


type alias WordRepository =
    Array String


type alias Model =
    { evaluatedWords : List Word
    , currentWords : Array Word
    , currentTypedChars : Array String
    , applicationStatus : ApplicationStatus
    , currentPosition : Int
    , mdl : Material.Model
    }
