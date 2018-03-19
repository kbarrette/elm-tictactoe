-- A tic-tac-toe board


module Board exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes
import Html.Events


type Square
    = X
    | O
    | Empty


type alias Board =
    Array Square


empty : Board
empty =
    Array.repeat 9 Empty


range : List Int
range =
    List.range 0 8


get : Board -> Int -> Square
get board index =
    Array.get index board
        |> Maybe.withDefault Empty


move : Square -> Board -> Int -> Board
move player board index =
    Array.set index player board


isEmptySquare : Board -> Int -> Bool
isEmptySquare board index =
    Array.get index board
        |> Maybe.withDefault Empty
        |> (==) Empty


isFull : Board -> Bool
isFull board =
    board
        |> Array.filter ((==) Empty)
        |> Array.isEmpty


isWin : Square -> Board -> Bool
isWin square board =
    -- Rows, columns, and diagonals
    (allEqual square board 1 0)
        || (allEqual square board 1 3)
        || (allEqual square board 1 6)
        || (allEqual square board 3 0)
        || (allEqual square board 3 1)
        || (allEqual square board 3 2)
        || (allEqual square board 4 0)
        || (allEqual square board 2 2)


allEqual : Square -> Board -> Int -> Int -> Bool
allEqual square board interval start =
    [ (get board start)
    , (get board (start + interval))
    , (get board (start + interval + interval))
    ]
        |> List.all ((==) square)


html : Board -> Html Int
html board =
    Html.table []
        [ row board 0
        , row board 1
        , row board 2
        ]


row : Board -> Int -> Html Int
row board index =
    Html.tr []
        [ cell board (index * 3)
        , cell board ((index * 3) + 1)
        , cell board ((index * 3) + 2)
        ]


cell : Board -> Int -> Html Int
cell board index =
    Html.td [ Html.Attributes.id (String.append "cell" (toString index)) ]
        [ Html.button
            [ Html.Events.onClick index
            , Html.Attributes.disabled (not (isEmptySquare board index))
            ]
            [ Html.text (cellDisplayString board index) ]
        ]


cellDisplayString : Board -> Int -> String
cellDisplayString board index =
    case get board index of
        Empty ->
            "?"

        X ->
            "X"

        O ->
            "O"
