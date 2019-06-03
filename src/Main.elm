-- Main.elm
module Main exposing (main)

import Browser
import Html exposing (Html, h1, text, div, table, thead, th, tr, td, ul, li, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import String
import Array

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model = game model

game model =
  div [] [
      h1 [] [ text "TTT in Elm" ]
    , div [] [ text "Winner: ", text <| if model.winner == "" then "?" else model.winner ]
    , div [] [ text "stepNumber: ", text <| String.fromInt model.stepNumber ]
    , div [] [ text "Moves used: ", text <| String.fromInt <| movesUsed model.board ]
    , div [] [ text "Moves left: ", text <| String.fromInt <| movesLeft model.board ]
    , div [] [ text "Player: ", text <| getPlayer model.board ]
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


-- MODEL

type alias Board = Array.Array String.String

type alias Model =
  { stepNumber: Int
  , board: Board
  , winner: String
  }

-- INIT

init : () -> (Model, Cmd Msg)
init _ =
  ( { stepNumber = 0
    , board = emptyBoard
    , winner = ""
    }
  , Cmd.none)


emptyBoard : Board
emptyBoard = Array.repeat 9 ""

-- UPDATE

type Msg = Cell Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if model.winner /= "" then
    (model, Cmd.none)
  else
    case msg of
      Cell index ->
        if cellAt index model.board /= "" then
            (model, Cmd.none)
        else
          let
            newBoard = Array.set index (getPlayer model.board) model.board
            newWinner = winner newBoard
            newModel = { model | board = newBoard, winner = newWinner }
          in
            (newModel, Cmd.none)

winner : Board -> String
winner brd =
  let
    wins : List (List Int)
    wins = [
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
    List.foldl (\cur acc -> let theWinner = findWinner cur in if theWinner /= "" then theWinner else acc) "" wins