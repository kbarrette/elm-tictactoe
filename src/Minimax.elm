-- A naive tic-tac-toe player


module Minimax exposing (..)

import Board exposing (Board, Square(..))


move : Board -> Maybe Int
move board =
    generateMoves board
        |> List.map (\m -> ( minimax Board.X (Board.move Board.O board m), m ))
        |> List.maximum
        |> Maybe.map Tuple.second


minimax : Square -> Board -> Int
minimax player board =
    case evaluate board of
        Just evaluation ->
            evaluation

        Nothing ->
            if player == Board.O then
                generateMoves board
                    |> List.map (Board.move Board.O board)
                    |> List.map (minimax Board.X)
                    |> List.maximum
                    |> Maybe.withDefault -1
            else
                generateMoves board
                    |> List.map (Board.move Board.X board)
                    |> List.map (minimax Board.O)
                    |> List.minimum
                    |> Maybe.withDefault 1


evaluate : Board -> Maybe Int
evaluate board =
    if Board.isWin Board.X board then
        Just -1
    else if Board.isWin Board.O board then
        Just 1
    else if Board.isFull board then
        Just 0
    else
        Nothing


generateMoves : Board -> List Int
generateMoves board =
    Board.range
        |> List.filter (Board.isEmptySquare board)
