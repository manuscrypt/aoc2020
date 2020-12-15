module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import List
import List.Extra as List


main : Html.Html msg
main =
    let
        localInput =
            input

        partA =
            solve (parseInput localInput) 2020

        partB =
            solve (parseInput localInput) 30000000
    in
    div []
        [ output "part1"
        , output (String.fromInt partA)
        , output "part2"
        , output (String.fromInt partB)
        ]


solve : List Int -> Int -> Int
solve startNums target =
    let
        ( lastSpoken, rest ) =
            startNums
                |> List.unconsLast
                |> Maybe.withDefault ( 0, [] )

        dict =
            rest
                |> List.indexedMap Tuple.pair
                |> List.foldl (\( i, num ) d -> Dict.insert num (i + 1) d) Dict.empty

        idx =
            List.length startNums
    in
    step idx target lastSpoken dict


step : Int -> Int -> Int -> Dict Int Int -> Int
step i target lastSpoken dict =
    if i < target then
        let
            ( nextSpoken, nextDict ) =
                turn i lastSpoken dict
        in
        step (i + 1) target nextSpoken nextDict

    else
        lastSpoken


turn : Int -> Int -> Dict Int Int -> ( Int, Dict Int Int )
turn index lastSpoken dict =
    let
        spoken =
            case Dict.get lastSpoken dict of
                Nothing ->
                    0

                Just lastIdx ->
                    index - lastIdx
    in
    ( spoken, Dict.insert lastSpoken index dict )


parseInput : String -> List Int
parseInput line =
    String.split "," line
        |> List.filterMap String.toInt


output : String -> Html msg
output s =
    div [] [ text s ]


sampleA : String
sampleA =
    """0,3,6"""


input : String
input =
    """13,16,0,12,15,1"""
