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
import Bounds exposing (get, ClientRect)
import Dom.Scroll exposing (..)


spaceKey =
    32


upperCaseA =
    65


lowerCaseZ =
    122


initialWordNumber =
    100


hardcodedWordRepository : Array String
hardcodedWordRepository =
    fromList [ "end", "start", "much", "dark", "better" ]


initialState : ( Model, Cmd Msg )
initialState =
    ( { evaluatedWords = []
      , currentTypedChars = fromList []
      , currentWords = fromList []
      , applicationStatus = Started
      , currentPosition = 0
      , currentSeed = initialSeed 0
      }
    , Cmd.batch [ timeForInitialSeed, getBoundsTask "typing" ]
    )


timeForInitialSeed : Cmd Msg
timeForInitialSeed =
    Task.perform TimeForInitialSeed Time.now


getBoundsTask : String -> Cmd Msg
getBoundsTask id =
    Bounds.get id |> Task.attempt processBounds


processBounds : Result String (Maybe ClientRect) -> Msg
processBounds result =
    case result of
        Ok result -> BoundsForElement result
        Err _ ->  NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TimeForInitialSeed time ->
            let
                randomWordsAndSeed =
                    randomWords initialWordNumber (initialSeedFromTime time) []

                wordList =
                    Tuple.first randomWordsAndSeed

                resultingSeed =
                    Tuple.second randomWordsAndSeed
            in
                ( { model | currentWords = fromList wordList, currentSeed = resultingSeed }, Cmd.none )

        KeyTyped key ->
            let
                keyPressed =
                    log "KeyCode pressed" (toString key)
            in
                if (key >= upperCaseA && key <= lowerCaseZ) then
                    updateCurrentTypedWords key model
                else if (key == spaceKey) then
                    let
                        modelStatus =
                            log "Current model after space pressed" model
                    in
                        updateWordStatus model
                            |> verifyNewWordsNeeded
                            |> wrapModelInCmd
                else
                    ( model, Cmd.none )


        BoundsForElement maybeBounds ->
            case maybeBounds of
                Nothing ->
                    let
                        nothingLog =
                            log "NOTHING HERE" ""
                    in
                        ( model, Cmd.none )

                Just bound ->
                    let
                        boundsLog =
                            log "RECT BOUNDS ARE" bound
                    in
                        ( model, Cmd.none )

        TestScroll ->
            ( model, testScroll )

        OnScrollFinished ->
            let
                x =
                    log "OnScrollFinished" ""
            in
                ( model, Cmd.none )


testScroll =
  Task.attempt (\_ -> OnScrollFinished) (toY "typing" 25)


wrapModelInCmd : Model -> ( Model, Cmd Msg )
wrapModelInCmd model =
    ( model, Cmd.none )


updateCurrentTypedWords : Int -> Model -> ( Model, Cmd Msg )
updateCurrentTypedWords keycode model =
    ( { model | currentTypedChars = push (fromChar (fromCode keycode)) model.currentTypedChars }, Cmd.none )


randomWords : Int -> Seed -> List Word -> ( List Word, Seed )
randomWords num seed acc =
    let
        arrayPosition =
            step (int 0 (Array.length hardcodedWordRepository - 1)) seed

        nextWord =
            createWord (extractText (Array.get (Tuple.first arrayPosition) hardcodedWordRepository))
    in
        if (List.length acc == num) then
            ( acc, (Tuple.second arrayPosition) )
        else
            randomWords num (Tuple.second arrayPosition) (acc ++ [ nextWord ])


createWord : String -> Word
createWord wordText =
    { text = wordText, wordStatus = Unevaluated, typedText = "" }


extractText : Maybe String -> String
extractText maybeWord =
    case maybeWord of
        Just w ->
            w

        Nothing ->
            ""


updateWordStatus : Model -> Model
updateWordStatus model =
    let
        currentWord =
            log "updateWordStatus extractWord = " (extractWord (Array.get model.currentPosition model.currentWords))

        currentWordStatus =
            log "updateWordStatus currentWordStatus = " (resolveWordStatus currentWord.text model.currentTypedChars)

        updatedWord =
            log "updateWordStatus updatedWord" (createWordWithUpdatedStatus currentWord model.currentTypedChars currentWordStatus)
    in
        { model
            | currentTypedChars = fromList []
            , currentWords = set model.currentPosition updatedWord model.currentWords
            , currentPosition = model.currentPosition + 1
        }


verifyNewWordsNeeded : Model -> Model
verifyNewWordsNeeded model =
    let
        remainingWordsToEvaluate =
            Array.length model.currentWords - (model.currentPosition + 1)
    in
        if (remainingWordsToEvaluate == 0) then
            let
                randomWordsAndSeed =
                    randomWords initialWordNumber model.currentSeed []

                wordList =
                    Tuple.first randomWordsAndSeed

                resultingSeed =
                    Tuple.second randomWordsAndSeed
            in
                { model | currentWords = Array.append (fromList wordList) model.currentWords, currentSeed = resultingSeed }
        else
            model


resolveWordStatus : String -> Array String -> WordStatus
resolveWordStatus originalText typedTextArray =
    let
        typedTextString =
            arrayToString typedTextArray
    in
        if (originalText == typedTextString) then
            TypedCorrectly
        else
            TypedIncorrectly


createWordWithUpdatedStatus : Word -> Array String -> WordStatus -> Word
createWordWithUpdatedStatus word typedText status =
    { word | wordStatus = status, typedText = arrayToString typedText }


extractWord : Maybe Word -> Word
extractWord maybeWord =
    case maybeWord of
        Just word ->
            word

        Nothing ->
            { text = "OOOPS", wordStatus = Unevaluated, typedText = "" }


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyTyped


initialSeedFromTime : Time -> Seed
initialSeedFromTime time =
    initialSeed (truncate (inMilliseconds time))


arrayToString : Array String -> String
arrayToString array =
    Array.foldr (++) "" array
