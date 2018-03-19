module MinimaxTests exposing (..)

import Minimax as Minimax
import Board as Board exposing (Board)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Minimax"
        [ describe "Minimax.generateMoves board"
            [ test "properly generates moves"
                (\_ ->
                    let
                        board =
                            List.repeat 9 Board.Empty

                        expected =
                            [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
                    in
                        Expect.equal expected (Minimax.generateMoves board)
                )
            ]
        , describe "Minimax.move board"
            [ test "chooses the quickest victory"
                (\_ ->
                    let
                        board =
                            [ [ Board.X, Board.Empty, Board.Empty ]
                            , [ Board.O, Board.O, Board.Empty ]
                            , [ Board.X, Board.Empty, Board.X ]
                            ]
                                |> List.concat
                    in
                        Expect.equal (Just 5) (Minimax.move board)
                )
            ]
        ]
