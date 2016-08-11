module App exposing (..)

import Html exposing (div, Html, text, button)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import List exposing (..)
import Html.App
import Keyboard
import Random exposing (Seed, initialSeed, step, int, list)
import Debug exposing (log, crash)
import Array exposing (Array, length, get, fromList, toList, push, set)
import Time exposing (now, inMilliseconds, Time)
import Task exposing (perform)
import Char exposing (fromCode, toCode)
import String exposing (fromChar)


type alias Word =
    { text : String
    , typedText : String
    , wordStatus : WordStatus
    }


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
    { wordsRepository : WordRepository
    , evaluatedWords : List Word
    , currentWords : Array Word
    , currentTypedChars : Array String
    , applicationStatus : ApplicationStatus
    , currentPosition : Int
    }


spaceKey =
    32


upperCaseA =
    65


lowerCaseZ =
    122

initalWordNumber =
    15


hardcodedWordRepository : Array String
hardcodedWordRepository =
    fromList [ "end", "start", "much", "dark", "better" ]


init : ( Model, Cmd Msg )
init =
    ( { wordsRepository = hardcodedWordRepository
      , evaluatedWords = []
      , currentTypedChars = fromList []
      , currentWords = fromList []
      , applicationStatus = Started
      , currentPosition = 0
      }
    , Task.perform (\_ -> crash "") (\time -> TimeForInitialSeed time) Time.now
    )



-- MESSAGES


type Msg
    = NoOp
    | TimeForInitialSeed Time
    | KeyTyped Keyboard.KeyCode



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "root"]
      [ div [ class "typing"]
            [
               div [] (wordsToHTML (model.currentWords))
             , div [] [ (text (arrayToString model.currentTypedChars)) ]
             ]
            ]



arrayToString : Array String -> String
arrayToString array =
    Array.foldr (++) "" array


wordsToHTML : Array Word -> List (Html.Html Msg)
wordsToHTML words =
    words |> Array.map (\w -> div
    [
        style [ getWordStyle w ]
      , class "typingText"
    ]
    [ text w.text ])
    |> toList

getWordStyle : Word -> (String, String)
getWordStyle word =
    case word.wordStatus of
        Unevaluated ->
            ("color", "black")
        TypedCorrectly ->
            ("color", "#7FFF00")
        TypedIncorrectly ->
            ("color", "red")

-- UPDATE


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

initalSeedFromTime : Time -> Seed
initalSeedFromTime time =
    initialSeed (truncate (inMilliseconds time))


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

extractWord : Maybe Word -> Word
extractWord maybeWord =
    case maybeWord of
        Just word ->
            word
        Nothing ->
            {text = "OOOPS", wordStatus = Unevaluated, typedText = ""}

createWordWithUpdatedStatus : Word -> Array String -> WordStatus -> Word
createWordWithUpdatedStatus word typedText status =
    { word | wordStatus = status, typedText = arrayToString typedText}

resolveWordStatus : String -> Array String -> WordStatus
resolveWordStatus originalText typedTextArray =
    let
       typedTextString = arrayToString typedTextArray
    in
        if (originalText == typedTextString) then
            TypedCorrectly
        else
            TypedIncorrectly

updateCurrentTypedWords : Int -> Model -> ( Model, Cmd Msg )
updateCurrentTypedWords keycode model =
    ( { model | currentTypedChars = push (fromChar (fromCode keycode)) model.currentTypedChars }, Cmd.none )


randomWords : Int -> Seed -> List Word -> List Word
randomWords num seed acc =
    let
        arrayPosition = step (int 0 (Array.length hardcodedWordRepository)) seed
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

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyTyped


main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
