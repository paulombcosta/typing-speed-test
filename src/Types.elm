module Types exposing (..)

import Array exposing (Array)
import Time exposing (Time)
import Keyboard
import Random exposing (initialSeed, Seed)
import Bounds exposing (ClientRect)


type alias Word =
    { text : String
    , typedText : String
    , wordStatus : WordStatus
    }


type Msg
    = NoOp
    | TimeForInitialSeed Time
    | BoundsForElement (Maybe ClientRect)
    | KeyTyped Keyboard.KeyCode
    | TestScroll
    | OnScrollFinished


type ApplicationStatus
    = Started
    | NotStarted
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
    , currentSeed : Seed
    , currentBound : ClientRect
    , currentYScroll : Float
    , lineScrollThreshold : Int
    , lineScrollAcc : Int
    , firstLineTyped : Bool
    }
