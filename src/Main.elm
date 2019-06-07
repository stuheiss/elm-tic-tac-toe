-- Main.elm
module Main exposing (main)

import Browser
import Html exposing (Html, h1, h2, text, div, table, thead, th, tr, td, ul, li, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import String
import Array
import Random
import Random.List exposing (shuffle)


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

type alias Model =
  { stepNumber: Int
  , board: Board
  , winner: String
  , automoves: List Int
  , rand: Int
  }


-- INIT


init : () -> (Model, Cmd Msg)
init _ =
  ( { stepNumber = 0
    , board = emptyBoard
    , winner = ""
    , automoves = List.range 0 8 -- ordered
    , rand = 0
    }
  , initCmd)

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
initCmd = generateNewRandomNumber

-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE


type Msg
  = Cell Int
  | Restart
  | Auto
  | GenerateRandomNumber
  | NewRandomNumber Int

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
          let (m2, c2) = update (Cell x) {model | automoves = xs}
          in (update Auto m2)
        _ ->
          (model, Cmd.none)
    Restart ->
      init ()
    Cell index ->
      if model.winner /= "" || cellAt index model.board /= "" then
          (model, Cmd.none)
      else
        let
          newBoard = Array.set index (getPlayer model.board) model.board
          newWinner = getWinner newBoard
          newModel = { model | board = newBoard, winner = newWinner }
        in
          (newModel, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  let
    status =
      if model.winner /= "" then
        "Winner: " ++ model.winner
      else if movesLeft model.board < 1 then
        "Tie Game"
      else
        "Player: " ++ getPlayer model.board
  in
    div [] [
        h1 [] [ text "TTT in Elm" ]
      , h2 [] [ text status ]
      , button [ onClick Restart ] [ text "Restart"]
      , button [ onClick GenerateRandomNumber ] [ text "New Battle Plan"]
      , button [ onClick Auto ] [ text "Battle"]
      , div [] [ text "automoves: ", text <| Debug.toString model.automoves ]
      , div [] [ text "rand: ", text <| String.fromInt model.rand ]
      , div [] [ text "Moves used: ", text <| String.fromInt <| movesUsed model.board ]
      , div [] [ text "Moves left: ", text <| String.fromInt <| movesLeft model.board ]
      , board model.board
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

board : Board -> Html Msg
board brd =
  table styledTable
    [ tr [] [ square 0 brd, square 1 brd, square 2 brd ]
    , tr [] [ square 3 brd, square 4 brd, square 5 brd ]
    , tr [] [ square 6 brd, square 7 brd, square 8 brd ]
    ]

square : Int -> Array.Array String.String -> Html Msg
square index brd =
  td (styledTd (Cell index)) [ text <| cellAt index brd ]

cellAt : Int -> Board -> String
cellAt idx brd = Array.get idx brd |> Maybe.withDefault "?"

styledTable = [
    style "width" "200px"
  , style "height" "200px"
  , style "border" "1px solid black"
  , style "border-collapse" "collapse"
  , style "text-align" "center"
  ]

styledTd msg = [
    style "border" "1px solid black"
  , style "font-size" "40px"
  , style "width" "33%"
  , style "height" "33%"
  , onClick msg
  ]

getWinner : Board -> String
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

    findWinner : List Int -> String
    findWinner tripple =
      case tripple of
        [a, b, c] ->
          let
            aVal = cellAt a brd
            bVal = cellAt b brd
            cVal = cellAt c brd
          in
            if aVal /= "" && aVal == bVal && aVal == cVal then
              aVal
            else
              ""
        _ -> ""
  in
    List.foldl (\cur acc -> let theWinner = findWinner cur in if theWinner /= "" then theWinner else acc) "" possibleWins
