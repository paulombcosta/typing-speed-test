module State exposing (..)

import Types exposing (..)

import List exposing (..)
import Keyboard
import Random exposing (Seed, initialSeed, step, int, list)
import Debug exposing (log, crash)
import Array exposing (Array, length, get, fromList, toList, push, set, indexedMap)
import Time exposing (now, inMilliseconds, Time)
import Task exposing (perform)
import Char exposing (fromCode)
import String exposing (fromChar)
import Types exposing (..)
import Material
import Material.Layout as Layout

spaceKey =
    32


upperCaseA =
    65


lowerCaseZ =
    122

initalWordNumber =
    40

hardcodedWordRepository : Array String
hardcodedWordRepository =
    fromList [ "end", "start", "much", "dark", "better" ]


initalState : (Model, Cmd Msg)
initalState =
    ( { evaluatedWords = []
      , currentTypedChars = fromList []
      , currentWords = fromList []
      , applicationStatus = Started
      , currentPosition = 0
      , mdl = Layout.setTabsWidth 2124 Material.model
      }
    , Cmd.batch [timeForInitialSeed, materialInit]
    )

timeForInitialSeed : Cmd Msg
timeForInitialSeed = Task.perform (\_ -> crash "") (\time -> TimeForInitialSeed time) Time.now

materialInit : Cmd Msg
materialInit = Material.init Mdl


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TimeForInitialSeed time ->
            ( { model | currentWords = fromList (randomWords initalWordNumber (initalSeedFromTime time) []) }, Cmd.none )

        KeyTyped key ->
            let
                keyPressed =
                    log "KeyCode pressed" (toString key)
            in
                if (key >= upperCaseA && key <= lowerCaseZ) then
                    updateCurrentTypedWords key model
                else if (key == spaceKey) then
                    let
                        modelStatus = log "Current model after space pressed" model
                    in
                        updateWordStatus model
                else
                    ( model, Cmd.none )

        Mdl msg ->
          Material.update msg model

updateCurrentTypedWords : Int -> Model -> ( Model, Cmd Msg )
updateCurrentTypedWords keycode model =
    ( { model | currentTypedChars = push (fromChar (fromCode keycode)) model.currentTypedChars }, Cmd.none )


randomWords : Int -> Seed -> List Word -> List Word
randomWords num seed acc =
    let
        arrayPosition = step (int 0 (Array.length hardcodedWordRepository - 1)) seed
        nextWord = createWord (extractText (get (fst arrayPosition) hardcodedWordRepository))
    in
        if (List.length acc == num) then
          acc
        else
          randomWords num (snd arrayPosition) (acc ++ [nextWord])


createWord : String -> Word
createWord wordText = { text = wordText, wordStatus = Unevaluated, typedText = "" }



extractText : Maybe String -> String
extractText maybeWord =
    case maybeWord of
        Just w ->
            w

        Nothing ->
            ""

updateWordStatus : Model -> (Model, Cmd Msg)
updateWordStatus model =
    let
        currentWord = log "updateWordStatus extractWord = " (extractWord (Array.get model.currentPosition model.currentWords))
        currentWordStatus = log "updateWordStatus currentWordStatus = " (resolveWordStatus currentWord.text model.currentTypedChars)
        updatedWord = log "updateWordStatus updatedWord" (createWordWithUpdatedStatus currentWord model.currentTypedChars currentWordStatus)
    in
        ({ model | currentTypedChars = fromList [""]
          , currentWords = set model.currentPosition updatedWord model.currentWords
          , currentPosition = model.currentPosition + 1 }
          , Cmd.none )


resolveWordStatus : String -> Array String -> WordStatus
resolveWordStatus originalText typedTextArray =
    let
       typedTextString = arrayToString typedTextArray
    in
        if (originalText == typedTextString) then
            TypedCorrectly
        else
            TypedIncorrectly



createWordWithUpdatedStatus : Word -> Array String -> WordStatus -> Word
createWordWithUpdatedStatus word typedText status =
    { word | wordStatus = status, typedText = arrayToString typedText}



extractWord : Maybe Word -> Word
extractWord maybeWord =
    case maybeWord of
        Just word ->
            word
        Nothing ->
            {text = "OOOPS", wordStatus = Unevaluated, typedText = ""}


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyTyped


initalSeedFromTime : Time -> Seed
initalSeedFromTime time =
    initialSeed (truncate (inMilliseconds time))



arrayToString : Array String -> String
arrayToString array =
    Array.foldr (++) "" array

