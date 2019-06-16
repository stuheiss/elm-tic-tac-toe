-- Main.elm
module Main exposing (main)

import Browser
import Html exposing (Html, h1, h2, h3, text, div, table, thead, th, tr, td, ul, li, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import String
import Array
import Random
import Random.List exposing (shuffle)
import Delay
import Time
import Task


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL


type alias Board = Array.Array String.String

type alias Winner =
  { player: String
  , indexes : List Int
  }

type alias Model =
  { stepNumber: Int
  , board: Board
  , winner: Winner
  , automoves: List Int
  , rand: Int
  , color : String
  , colorCycling : Bool
  , zone : Time.Zone
  , time : Time.Posix
  , string: String
  }


-- INIT


init : () -> (Model, Cmd Msg)
init _ =
  ( { stepNumber = 0
    , board = emptyBoard
    , winner = initWinner
    , automoves = List.range 0 8 -- ordered
    , rand = 0
    , color = ""
    , colorCycling = False
    , zone = Time.utc
    , time = Time.millisToPosix 0
    , string = ""
    }
  , initCmd)

initWinner : Winner
initWinner = { player = "", indexes = [-1, -1, -1] }

generateNewAutomoves : Int -> List Int
generateNewAutomoves seed =
  let
      initialList = List.range 0 8
      ( shuffledList, _ ) = Random.step (Random.List.shuffle initialList) (Random.initialSeed seed)
  in
    shuffledList

emptyBoard : Board
emptyBoard = Array.repeat 9 ""

generateNewRandomNumber : Cmd Msg
generateNewRandomNumber = (Random.generate NewRandomNumber (Random.int 0 Random.maxInt))

-- get an initial random number in model.rand and update model.automoves
initCmd : Cmd Msg
initCmd = Cmd.batch
    [ Task.perform AdjustTimeZone Time.here -- get current tz
    , Task.perform Tick Time.now            -- get current localtime
    , generateNewRandomNumber
    ]


-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- UPDATE


type Msg
  = Cell Int
  | Restart
  | Auto
  | GenerateRandomNumber
  | NewRandomNumber Int
  | Trigger
  | Red
  | Green
  | Blue
  | White
  | ColorCycling Bool
  | Tick Time.Posix
  | AdjustTimeZone Time.Zone



cycleColors : Model -> Cmd Msg
cycleColors model =
    Delay.sequenceIf (not model.colorCycling) <|
        Delay.withUnit Delay.Millisecond
            [ ( 0, ColorCycling True )
            , ( 0, Red )
            , ( 2000, Green )
            , ( 2000, Blue )
            , ( 2000, White )
            , ( 2000, ColorCycling False )
            ]


{--
after : Float -> TimeUnit -> msg -> Cmd msg
Delays an update (with a message) by a given amount of time

after 500 Millisecond DelayedMsg

To trigger a single update after a period of time pass Delay.after as a command to the elm runtime:

FirstMessage ->
    model ! [ Delay.after 500 Millisecond SecondMessage ]
--}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenerateRandomNumber ->
      ( model, generateNewRandomNumber )

    NewRandomNumber newRandomNumber ->
      ( {model | rand = newRandomNumber, automoves = generateNewAutomoves newRandomNumber}, Cmd.none )

    Auto ->
      case model.automoves of
        x :: xs ->
          let
            (newModel, newCmd) = update (Cell x) {model | automoves = xs}
          in
            (newModel, Delay.after 500 Delay.Millisecond Auto)
        _ ->
          (model, Cmd.none)

    Restart ->
      init ()

    Cell index ->
      if model.winner.player /= "" || cellAt index model.board /= "" then
          (model, Cmd.none)
      else
        let
          newBoard = Array.set index (getPlayer model.board) model.board
          newWinner = getWinner newBoard
          newModel = { model | board = newBoard, winner = newWinner }
          newCmd =
            if newWinner.player == "" then
              Cmd.none
            else
              cycleColors model
        in
          (newModel, Cmd.none)

    Trigger ->
        ( model, cycleColors model )

    Red ->
        ( { model | color = "#BC1F31" }, Cmd.none )

    Green ->
        ( { model | color = "#259860" }, Cmd.none )

    Blue ->
        ( { model | color = "#1E70B5" }, Cmd.none )

    White ->
        ( { model | color = "" }, Cmd.none )

    ColorCycling bool ->
        ( { model | colorCycling = bool }, Cmd.none )

    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )


-- VIEW


view : Model -> Html Msg
view model =
  let
    status =
      if model.winner.player /= "" then
        "Winner: " ++ model.winner.player
      else if movesLeft model.board < 1 then
        "Tie Game"
      else
        "Player: " ++ getPlayer model.board
    hour   = String.fromInt (Time.toHour   model.zone model.time)
    minute = String.fromInt (Time.toMinute model.zone model.time)
    second = String.fromInt (Time.toSecond model.zone model.time)
    hhmmss = hour ++ ":" ++ minute ++ ":" ++ second
  in
    div [] [
        h1 [] [ text "Tic Tac Toe in Elm" ]
      , h3 [] [ text hhmmss ]
      , h2 [] [ text status ]
      , button [ onClick Restart ] [ text "Restart"]
      , button [ onClick GenerateRandomNumber ] [ text "New Battle Plan"]
      , button [ onClick Auto ] [ text "Battle"]
      , button [ onClick Trigger ] [ text "Cycle color" ]
      , div [] [ text "battleplan: ", text <| Debug.toString model.automoves ]
      -- , div [] [ text "winner: ", text <| Debug.toString model.winner ]
      -- , div [] [ text "rand: ", text <| String.fromInt model.rand ]
      , div [] [ text "Moves used: ", text <| String.fromInt <| movesUsed model.board ]
      , div [] [ text "Moves left: ", text <| String.fromInt <| movesLeft model.board ]
      , board model
    ]

movesUsed : Board -> Int
movesUsed brd =
  Array.foldl (\cur acc -> acc ++ cur) "" brd |> String.length

movesLeft : Board -> Int
movesLeft brd =
  Array.length brd - movesUsed brd

getPlayer : Board -> String
getPlayer brd =
  case modBy 2 (movesUsed brd) of
    0 -> "X"
    _ -> "O"

board : Model -> Html Msg
board model =
  table (styledTable model)
    [ tr [] (List.map (\i -> square i model) [0, 1, 2])
    , tr [] (List.map (\i -> square i model) [3, 4, 5])
    , tr [] (List.map (\i -> square i model) [6, 7, 8])
    ]

square : Int -> Model -> Html Msg
square index model =
  td (styledTd index model (Cell index)) [ text <| cellAt index model.board ]

cellAt : Int -> Board -> String
cellAt idx brd = Array.get idx brd |> Maybe.withDefault "?"

styledTable model =
  [ style "width" "200px"
  , style "height" "200px"
  , style "border" "1px solid black"
  , style "border-collapse" "collapse"
  , style "text-align" "center"
  ]

styledTd idx model msg =
  [ style "border" "1px solid black"
  , style "font-size" "40px"
  , style "width" "33%"
  , style "height" "33%"
  , style "background-color" (playerBackgroundColor idx model)
  , style "animation-duration" "2s"
  , style "color" model.color
  , class (playerClass idx model)
  , onClick msg
  ]

playerClass idx model =
  let
      player = cellAt idx model.board
  in
    if player == "" then
      "w3-container"
    else if player == model.winner.player && (List.member idx model.winner.indexes) then
      "w3-container w3-animate-fading"
    else
      "w3-container w3-animate-opacity"

playerBackgroundColor idx model =
  case cellAt idx model.board of
    "X" -> "lightgreen"
    "O" -> "lightblue"
    _ -> "white"

getWinner : Board -> Winner
getWinner brd =
  let
    possibleWins : List (List Int)
    possibleWins = [
      [0, 1, 2],
      [3, 4, 5],
      [6, 7, 8],
      [0, 3, 6],
      [1, 4, 7],
      [2, 5, 8],
      [0, 4, 8],
      [2, 4, 6]
      ]

    findWinner : List Int -> Winner
    findWinner indexes =
      case indexes of
        [a, b, c] ->
          let
            aVal = cellAt a brd
            bVal = cellAt b brd
            cVal = cellAt c brd
          in
            if aVal /= "" && aVal == bVal && aVal == cVal then
              { player = aVal, indexes = indexes }
            else
              initWinner
        _ -> initWinner
  in
    List.foldl (\cur acc -> let winner = findWinner cur in if winner.player /= "" then winner else acc) initWinner possibleWins
