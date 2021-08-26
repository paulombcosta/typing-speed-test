module Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Element exposing (rgb255)
import Random exposing (Seed)
import Time exposing (Posix)


type alias Word =
    { text : String
    , typedText : String
    , wordStatus : WordStatus
    }


type Msg
    = NoOp
    | TimeForInitialSeed Posix
    | KeyTyped String
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


colors =
    { black = rgb255 0 0 0
    , green = rgb255 127 255 0
    , red = rgb255 255 0 0
    , white = rgb255 255 255 255
    , blue = rgb255 106 139 227
    , darkBg = rgb255 50 52 55
    , wordCorrect = rgb255 209 208 197
    , wordDefault = rgb255 100 102 105
    , wordErr = rgb255 202 71 84
    , caretColor = rgb255 226 183 20
    }


type alias Model =
    { currentTypedChars : Array String
    , applicationStatus : ApplicationStatus
    , currentSeed : Seed
    , firstLineTyped : Bool
    , timeLimitSeconds : Int
    , timePassedSeconds : Int
    , currentWPM : Int
    , currentCPM : Int
    , currentWordIndex : Int
    , currentRowIndex : Int
    , wordsPerRow : Int
    , numRows : Int
    , rows : Dict Int (Array Word)
    , evaluatedWords : List Word
    }
