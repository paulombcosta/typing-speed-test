module Types exposing (..)

import Array exposing (Array)
import Random exposing (Seed, initialSeed)
import Time exposing (Posix)
import Bounds exposing (BoundingClientRect)


type alias Word =
    { text : String
    , typedText : String
    , wordStatus : WordStatus
    }


type Msg
    = NoOp
    | TimeForInitialSeed Posix
    | SetBoundingClientRect (Maybe BoundingClientRect)
    | KeyTyped String
    | TestScroll
    | OnScrollFinished
    | Tick Posix
    | StartApp
    | Restart


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
    , currentBound : BoundingClientRect
    , currentYScroll : Float
    , lineScrollThreshold : Int
    , lineScrollAcc : Int
    , firstLineTyped : Bool
    , timeLimitSeconds : Int
    , timePassedSeconds : Int
    , currentWPM : Int
    , currentCPM : Int
    }
