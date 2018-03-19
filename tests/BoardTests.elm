module BoardTests exposing (..)

import Board as Board exposing (Board)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Board"
        [ describe "Board.move index player board"
            [ test "properly adds moves at the start of the board"
                (\_ ->
                    let
                        board =
                            List.repeat 9 Board.Empty

                        expected =
                            Board.X :: List.repeat 8 Board.Empty
                    in
                        Expect.equal expected (Board.move Board.X board 0)
                )
            , test "properly adds moves at the end of the board"
                (\_ ->
                    let
                        board =
                            List.repeat 9 Board.Empty

                        expected =
                            List.concat
                                [ List.repeat 8 Board.Empty
                                , [ Board.X ]
                                ]
                    in
                        Expect.equal expected (Board.move Board.X board 8)
                )
            ]
        ]
