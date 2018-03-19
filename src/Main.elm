module Main exposing (..)

import Board exposing (Board, Square(..))
import Minimax exposing (move)
import Html exposing (Html)
import Html.Attributes


-- MODEL


type alias Model =
    Board



-- UPDATE


type alias Msg =
    Int


update : Msg -> Model -> Model
update msg model =
    let
        updatedBoard =
            Board.move X model msg
    in
        case Minimax.move updatedBoard of
            Just computerMove ->
                Board.move O updatedBoard computerMove

            Nothing ->
                updatedBoard



-- VIEW


view : Board -> Html Msg
view board =
    Html.div
        [ Html.Attributes.id "container" ]
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "css/styles.css" ] []
        , Html.div [ Html.Attributes.id "board" ]
            [ Board.html board
            , if Board.isWin O board then
                Html.div [ Html.Attributes.id "overlay" ] [ Html.text "YOU LOSE" ]
              else if Board.isFull board then
                Html.div [ Html.Attributes.id "overlay" ] [ Html.text "DRAW" ]
              else
                Html.div [] []
            ]
        ]



-- PROGRAM


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = Board.empty
        , view = view
        , update = update
        }
