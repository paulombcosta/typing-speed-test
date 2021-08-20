module State exposing (..)

import Array exposing (Array, fromList, get, indexedMap, length, push, set, toList)
import Bounds exposing (BoundingClientRect, fetchBoundingClientRect, origin, setBoundingClientRect)
import Browser.Dom as Dom
import Browser.Events exposing (onKeyPress)
import Char exposing (fromCode)
import Json.Decode as Decode
import List exposing (..)
import Random exposing (Seed, initialSeed, int, list, step)
import String exposing (fromChar, fromInt)
import Task exposing (perform)
import Time exposing (Posix, every, now, posixToMillis)
import Types exposing (..)
import Words exposing (words)


spaceKey =
    32


upperCaseA =
    65


lowerCaseZ =
    122


initialWordNumber =
    100


lineHeight =
    46


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
    , Cmd.batch [ timeForInitialSeed, scrollY 0 ]
    )


timeForInitialSeed : Cmd Msg
timeForInitialSeed =
    Task.perform TimeForInitialSeed now


getBoundsTask : String -> Cmd Msg
getBoundsTask id =
    fetchBoundingClientRect id


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
                code =
                    case String.uncons key of
                        Nothing ->
                            Nothing

                        Just ( char, str ) ->
                            Just (Char.toCode char)

                newModel =
                    { model | applicationStatus = Started }
            in
            case code of
                Nothing ->
                    ( model, Cmd.none )

                Just keyCode ->
                    if keyCode >= upperCaseA && keyCode <= lowerCaseZ then
                        updateCurrentTypedWords keyCode newModel

                    else if keyCode == spaceKey then
                        updateWordStatus newModel
                            |> verifyNewWordsNeeded
                            |> wrapModelInCmd

                    else
                        ( model, Cmd.none )

        SetBoundingClientRect maybeBounds ->
            let
                lineChanged =
                    checkLineChanged model.currentBound maybeBounds

                newModel =
                    case maybeBounds of
                        Nothing ->
                            model

                        Just bounds ->
                            if lineChanged then
                                if model.firstLineTyped == False then
                                    { model
                                        | currentBound = bounds
                                        , lineScrollAcc = model.lineScrollAcc + 1
                                        , firstLineTyped = True
                                    }

                                else
                                    { model
                                        | currentBound = bounds
                                        , lineScrollAcc = model.lineScrollAcc + 1
                                        , lineScrollThreshold = 1
                                    }

                            else
                                { model | currentBound = bounds }
            in
            if shouldScroll newModel then
                let
                    currentYScroll =
                        model.currentYScroll + lineHeight
                in
                ( { newModel | lineScrollAcc = 0, currentYScroll = currentYScroll }, scrollY currentYScroll )

            else
                ( newModel, Cmd.none )

        TestScroll ->
            let
                currentScroll =
                    model.currentYScroll + lineHeight
            in
            ( { model | currentYScroll = currentScroll }, scrollY currentScroll )

        OnScrollFinished ->
            ( model, Cmd.none )

        Tick time ->
            if (model.timePassedSeconds + 1) >= model.timeLimitSeconds then
                ( { model | applicationStatus = Finished }, Cmd.none )

            else
                ( { model | timePassedSeconds = model.timePassedSeconds + 1 }, Cmd.none )

        StartApp ->
            ( { model | applicationStatus = Started }, Cmd.none )

        Restart ->
            initialState


shouldScroll : Model -> Bool
shouldScroll model =
    if model.lineScrollAcc >= model.lineScrollThreshold then
        True

    else
        False


checkLineChanged : BoundingClientRect -> Maybe BoundingClientRect -> Bool
checkLineChanged previousViewport currentViewport =
    case currentViewport of
        Nothing ->
            False

        Just current ->
            if previousViewport /= Bounds.origin && current.top > previousViewport.top then
                True

            else
                False


scrollY y =
    -- Task.attempt (\_ -> OnScrollFinished) (toY "typing" y)
    Task.attempt (\_ -> OnScrollFinished) (Dom.setViewportOf "typing" 0 y)


wrapModelInCmd : Model -> ( Model, Cmd Msg )
wrapModelInCmd model =
    let
        currentWordClass =
            model.currentPosition
                |> fromInt
                |> String.append "word-"
    in
    ( model, getBoundsTask currentWordClass )


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
    if List.length acc == num then
        ( acc, Tuple.second arrayPosition )

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
            extractWord (Array.get model.currentPosition model.currentWords)

        currentWordStatus =
            resolveWordStatus currentWord.text model.currentTypedChars

        updatedWord =
            createWordWithUpdatedStatus currentWord model.currentTypedChars currentWordStatus
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
    if remainingWordsToEvaluate == 0 then
        let
            randomWordsAndSeed =
                randomWords initialWordNumber model.currentSeed []

            wordList =
                Tuple.first randomWordsAndSeed

            resultingSeed =
                Tuple.second randomWordsAndSeed
        in
        { model | currentWords = Array.append model.currentWords (fromList wordList), currentSeed = resultingSeed }

    else
        model


resolveWordStatus : String -> Array String -> WordStatus
resolveWordStatus originalText typedTextArray =
    let
        typedTextString =
            arrayToString typedTextArray
    in
    if originalText == typedTextString then
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


second : Float
second =
    1000


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map (\value -> KeyTyped value) (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.applicationStatus of
        Started ->
            Sub.batch [ onKeyPress keyDecoder, every second Tick, setBoundingClientRect SetBoundingClientRect ]

        NotStarted ->
            Sub.batch [ onKeyPress keyDecoder ]

        Finished ->
            Sub.none


initialSeedFromTime : Posix -> Seed
initialSeedFromTime time =
    initialSeed <| posixToMillis time


arrayToString : Array String -> String
arrayToString array =
    Array.foldr (++) "" array
