module Main exposing (main)

import Dict.Any as Dict exposing (AnyDict)
import Html exposing (Html, div, text)
import List.Extra as List


type CellState
    = Active
    | Inactive


type alias Coord =
    { x : Int
    , y : Int
    , z : Int
    , w : Int
    }


type alias Cell =
    { coord : Coord
    , state : CellState
    }


type alias Model =
    AnyDict String Coord CellState


main : Html.Html msg
main =
    let
        partA =
            cycleA 6 (parseInput input)
                |> Dict.values
                |> count Active

        partB =
            cycleB 6 (parseInput input)
                |> Dict.values
                |> count Active
    in
    div []
        [ output "part1"
        , output (String.fromInt partA)
        , output "part2"
        , output (String.fromInt partB)
        ]


cycleA : Int -> Model -> Model
cycleA c model =
    if c <= 0 then
        model

    else
        let
            min =
                mins model

            max =
                maxs model

            coords =
                List.range min.x max.x
                    |> List.concatMap
                        (\x ->
                            List.range min.y max.y
                                |> List.concatMap
                                    (\y ->
                                        List.range min.z max.z
                                            |> List.map
                                                (\z ->
                                                    Coord x y z 0
                                                )
                                    )
                        )
        in
        recalc coords model
            |> cycleA (c - 1)


cycleB : Int -> Model -> Model
cycleB c model =
    if c <= 0 then
        model

    else
        let
            min =
                mins model

            max =
                maxs model

            coords =
                List.range min.x max.x
                    |> List.concatMap
                        (\x ->
                            List.range min.y max.y
                                |> List.concatMap
                                    (\y ->
                                        List.range min.z max.z
                                            |> List.concatMap
                                                (\z ->
                                                    List.range min.w max.w
                                                        |> List.map (\w -> Coord x y z w)
                                                )
                                    )
                        )
        in
        recalc coords model
            |> cycleB (c - 1)


recalc : List Coord -> Model -> Model
recalc coords model =
    coords
        |> List.foldl
            (\coord dict ->
                let
                    cell =
                        Dict.get coord model
                            |> Maybe.withDefault Inactive

                    neighs =
                        neighbours coord model
                in
                Dict.insert coord (toggleIfNeeded neighs cell) dict
            )
            (Dict.empty coordToString)


toggleIfNeeded : List CellState -> CellState -> CellState
toggleIfNeeded neighs cellState =
    let
        nActive =
            count Active neighs
    in
    if cellState == Active then
        if nActive == 2 || nActive == 3 then
            cellState

        else
            Inactive

    else if nActive == 3 then
        Active

    else
        cellState


mins : Model -> Coord
mins model =
    let
        coords =
            model |> Dict.filter (\_ v -> v == Active) |> Dict.keys
    in
    { x = minAct .x coords, y = minAct .y coords, z = minAct .z coords, w = minAct .w coords }


maxs : Model -> Coord
maxs model =
    let
        coords =
            model |> Dict.filter (\_ v -> v == Active) |> Dict.keys
    in
    { x = maxAct .x coords, y = maxAct .y coords, z = maxAct .z coords, w = maxAct .w coords }


minAct : (Coord -> Int) -> List Coord -> Int
minAct func cells =
    cells
        |> List.map func
        |> List.minimum
        |> Maybe.map (\min -> min - 1)
        |> Maybe.withDefault 0


maxAct : (Coord -> Int) -> List Coord -> Int
maxAct func cells =
    cells
        |> List.map func
        |> List.maximum
        |> Maybe.map (\max -> max + 1)
        |> Maybe.withDefault 0


neighbours : Coord -> Model -> List CellState
neighbours start model =
    relCoords start
        |> List.map
            (\coord ->
                Dict.get coord model
                    |> Maybe.withDefault Inactive
            )


relCoords : Coord -> List Coord
relCoords { x, y, z, w } =
    List.range -1 1
        |> List.concatMap
            (\cx ->
                List.range -1 1
                    |> List.concatMap
                        (\cy ->
                            List.range -1 1
                                |> List.concatMap
                                    (\cz ->
                                        List.range -1 1
                                            |> List.map (\cw -> Coord cx cy cz cw)
                                    )
                        )
            )
        |> List.filter (\c -> not (c.x == 0 && c.y == 0 && c.z == 0 && c.w == 0))
        |> List.map (\c -> { x = x + c.x, y = y + c.y, z = z + c.z, w = w + c.w })


count : CellState -> List CellState -> Int
count state cells =
    cells |> List.filter ((==) state) |> List.length


output : String -> Html msg
output s =
    div [] [ text s ]


parseInput : String -> Model
parseInput str =
    str
        |> String.lines
        |> List.indexedMap parseRow
        |> List.concat
        |> List.map (\c -> ( c.coord, c.state ))
        |> Dict.fromList coordToString


coordToString : Coord -> String
coordToString c =
    String.fromInt c.x ++ "," ++ String.fromInt c.y ++ "," ++ String.fromInt c.z ++ "," ++ String.fromInt c.w


parseRow : Int -> String -> List Cell
parseRow y line =
    line
        |> String.toList
        |> List.indexedMap (parseColumn y)
        |> List.filterMap identity


parseColumn : Int -> Int -> Char -> Maybe Cell
parseColumn y x char =
    let
        cell =
            Cell { x = x, y = y, z = 0, w = 0 }
    in
    case char of
        '#' ->
            Just (cell Active)

        '.' ->
            Just (cell Inactive)

        _ ->
            Nothing


sample : String
sample =
    """.#.
..#
###"""


input : String
input =
    """##..####
.###....
#.###.##
#....#..
...#..#.
#.#...##
..#.#.#.
.##...#."""
