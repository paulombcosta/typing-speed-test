module State exposing (..)

import Types exposing (..)
import List exposing (..)
import Keyboard
import Random exposing (Seed, initialSeed, step, int, list)
import Debug exposing (log, crash)
import Array exposing (Array, length, get, fromList, toList, push, set, indexedMap)
import Time exposing (now, inMilliseconds, Time, every, second)
import Task exposing (perform)
import Char exposing (fromCode)
import String exposing (fromChar)
import Types exposing (..)
import Bounds exposing (get, ClientRect, origin)
import Dom.Scroll exposing (toY)
import Words exposing (words)


spaceKey =
    32


upperCaseA =
    65


lowerCaseZ =
    122


initialWordNumber =
    100


lineHeight = 56

hardcodedWordRepository : Array String
hardcodedWordRepository =
    fromList words


initialState : ( Model, Cmd Msg )
initialState =
    ( { evaluatedWords = []
      , currentTypedChars = fromList []
      , currentWords = fromList []
      , applicationStatus = NotStarted
      , currentPosition = 0
      , currentSeed = initialSeed 0
      , currentBound = Bounds.origin
      , currentYScroll = 0
      , lineScrollThreshold = 2
      , lineScrollAcc = 0
      , firstLineTyped = False
      , timeLimitSeconds = 60
      , timePassedSeconds = 0
      , currentWPM = 0
      , currentCPM = 0
      }
    , Cmd.batch [ timeForInitialSeed ]
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
                            log "Unable to fetch bounds" ""
                    in
                        ( model, Cmd.none )

                Just bound ->
                    let
                        lineChanged = checkLineChanged model.currentBound bound
                        newModel = if lineChanged then
                            if model.firstLineTyped == False then
                                {model | currentBound = bound
                                , lineScrollAcc = model.lineScrollAcc + 1
                                , firstLineTyped = True
                                }
                            else
                                {model | currentBound = bound
                                , lineScrollAcc = model.lineScrollAcc + 1
                                , lineScrollThreshold = 1
                                }
                          else
                            {model | currentBound = bound}
                    in
                        if shouldScroll newModel then
                            let
                              currentYScroll = (model.currentYScroll + lineHeight)
                            in
                                ( {newModel | lineScrollAcc = 0, currentYScroll = currentYScroll}, scrollY currentYScroll )
                        else
                            ( newModel, Cmd.none )

        TestScroll ->
            let
                currentScroll = (model.currentYScroll + lineHeight)
             in
                ( {model | currentYScroll = currentScroll}, scrollY currentScroll )

        OnScrollFinished ->
            let
                x =
                    log "OnScrollFinished" ""
            in
                ( model, Cmd.none )

        Tick time ->
            if (model.timePassedSeconds + 1) >= model.timeLimitSeconds then
              ( {model | applicationStatus = Finished }, Cmd.none)
            else
              ( {model | timePassedSeconds = model.timePassedSeconds + 1}, Cmd.none )

        StartApp ->
            ( {model | applicationStatus = Started}, Cmd.none )


shouldScroll : Model -> Bool
shouldScroll model =
    if model.lineScrollAcc >= model.lineScrollThreshold then True else False

checkLineChanged : ClientRect -> ClientRect -> Bool
checkLineChanged previousBound currentBound =
    if previousBound /= Bounds.origin && currentBound.top > previousBound.top then True else False


scrollY y =
  Task.attempt (\_ -> OnScrollFinished) (toY "typing" y)


wrapModelInCmd : Model -> ( Model, Cmd Msg )
wrapModelInCmd model =
    let
      currentWordClass = model.currentPosition
      |> toString
      |> String.append "word-"
    in
    ( model, getBoundsTask currentWordClass)


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
                { model | currentWords = Array.append model.currentWords (fromList wordList) , currentSeed = resultingSeed }
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
            { text = "", wordStatus = Unevaluated, typedText = "" }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.applicationStatus of
    Started ->
        -- Sub.batch [Keyboard.presses KeyTyped, every second Tick]
        Sub.batch [Keyboard.presses KeyTyped]
    _ -> Sub.none


initialSeedFromTime : Time -> Seed
initialSeedFromTime time =
    initialSeed (truncate (inMilliseconds time))


arrayToString : Array String -> String
arrayToString array =
    Array.foldr (++) "" array
