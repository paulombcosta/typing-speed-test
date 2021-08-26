module State exposing (..)

import Array exposing (Array, fromList, push)
import Browser.Events exposing (onKeyPress)
import Char exposing (fromCode)
import Dict
import Json.Decode as Decode
import List exposing (..)
import Random exposing (Seed, initialSeed, int, step)
import String exposing (fromChar)
import Task
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


hardcodedWordRepository : Array String
hardcodedWordRepository =
    fromList words


initialState : ( Model, Cmd Msg )
initialState =
    ( { currentTypedChars = fromList []
      , applicationStatus = NotStarted
      , currentSeed = initialSeed 0
      , firstLineTyped = False
      , timeLimitSeconds = 30
      , timePassedSeconds = 0
      , currentWPM = 0
      , currentCPM = 0
      , currentWordIndex = 0
      , currentRowIndex = 0
      , wordsPerRow = 15
      , numRows = 3
      , rows = Dict.empty
      , evaluatedWords = []
      }
    , Cmd.batch [ timeForInitialSeed ]
    )


timeForInitialSeed : Cmd Msg
timeForInitialSeed =
    Task.perform TimeForInitialSeed now


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TimeForInitialSeed time ->
            let
                randomWordsAndSeed =
                    randomWords initialWordNumber (initialSeedFromTime time) []

                wordArray =
                    Tuple.first randomWordsAndSeed
                        |> Array.fromList

                resultingSeed =
                    Tuple.second randomWordsAndSeed

                rows =
                    range 0 (model.numRows - 1)
                        |> List.map (\idx -> ( idx, Array.slice (model.wordsPerRow * idx) (model.wordsPerRow * (idx + 1)) wordArray ))
                        |> Dict.fromList
            in
            ( { model | currentSeed = resultingSeed, rows = rows }, Cmd.none )

        KeyTyped key ->
            let
                code =
                    String.uncons key
                        |> Maybe.map (\x -> Tuple.first x)
                        |> Maybe.map Char.toCode

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
                            |> (\m -> Tuple.pair m Cmd.none)

                    else
                        ( model, Cmd.none )

        Tick _ ->
            if (model.timePassedSeconds + 1) >= model.timeLimitSeconds then
                ( { model | applicationStatus = Finished }, Cmd.none )

            else
                ( { model | timePassedSeconds = model.timePassedSeconds + 1 }, Cmd.none )

        StartApp ->
            ( { model | applicationStatus = Started }, Cmd.none )

        Restart ->
            initialState


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
extractText word =
    Maybe.withDefault "" word


updateWordStatus : Model -> Model
updateWordStatus model =
    let
        row =
            model.currentRowIndex

        wordIndex =
            model.currentWordIndex

        currentWord =
            model.rows
                |> Dict.get row
                |> Maybe.andThen (Array.get wordIndex)
                |> Maybe.withDefault { text = "", wordStatus = Unevaluated, typedText = "" }

        currentWordStatus =
            resolveWordStatus currentWord.text model.currentTypedChars

        updatedWord =
            createWordWithUpdatedStatus currentWord model.currentTypedChars currentWordStatus

        updatedRows =
            Dict.get row model.rows
                |> Maybe.withDefault Array.empty
                |> Array.set wordIndex updatedWord
                |> (\v -> Dict.insert row v model.rows)
    in
    { model
        | currentTypedChars = fromList []
        , rows = updatedRows
        , evaluatedWords = updatedWord :: model.evaluatedWords
    }


verifyNewWordsNeeded : Model -> Model
verifyNewWordsNeeded model =
    let
        wordIdx =
            if model.currentWordIndex == model.wordsPerRow - 1 then
                0

            else
                model.currentWordIndex + 1

        rowIdx =
            if model.currentWordIndex == model.wordsPerRow - 1 then
                model.currentRowIndex + 1

            else
                model.currentRowIndex

        newModel =
            if rowIdx == model.numRows - 1 then
                let
                    ( newWords, seed ) =
                        randomWords model.wordsPerRow model.currentSeed []

                    newRows =
                        model.rows
                            -- 1st row becomes 2nd
                            |> Dict.insert 0 (Maybe.withDefault Array.empty (Dict.get 1 model.rows))
                            -- 2nd row becomes 3rd
                            |> Dict.insert 1 (Maybe.withDefault Array.empty (Dict.get 2 model.rows))
                            -- 3rd becomes a new list
                            |> Dict.insert 2 (fromList newWords)
                in
                { model | rows = newRows, currentWordIndex = wordIdx, currentRowIndex = rowIdx - 1, currentSeed = seed }

            else
                { model | currentWordIndex = wordIdx, currentRowIndex = rowIdx }
    in
    newModel


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
            Sub.batch [ onKeyPress keyDecoder, every second Tick ]

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
