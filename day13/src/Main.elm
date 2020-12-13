module Main exposing (main)

import Html exposing (Html, div, text)
import List exposing (product)
import List.Extra as List


type Bus
    = Relevant Int
    | Irrelevant


main : Html.Html msg
main =
    let
        ( arrivalTime, busLines ) =
            parseInput input

        partA =
            let
                ( t, bus ) =
                    findNextBus arrivalTime busLines
            in
            (t - arrivalTime) * bus

        partB =
            scheduleAtTime 0 busLines
    in
    div []
        [ output "part1"
        , output (String.fromInt partA)
        , output "part2"
        , output (String.fromInt partB)
        ]


findNextBus : Int -> List Bus -> ( Int, Int )
findNextBus t buses =
    case
        buses
            |> List.filterMap
                (\bus ->
                    case bus of
                        Irrelevant ->
                            Nothing

                        Relevant b ->
                            if modBy b t == 0 then
                                Just b

                            else
                                Nothing
                )
            |> List.head
    of
        Nothing ->
            findNextBus (t + 1) buses

        Just bus ->
            ( t, bus )


scheduleAtTime : Int -> List Bus -> Int
scheduleAtTime t buses =
    let
        matches =
            buses
                |> List.indexedMap
                    (\i b ->
                        case b of
                            Irrelevant ->
                                Nothing

                            Relevant busTime ->
                                if modBy busTime (t + i) == 0 then
                                    Just busTime

                                else
                                    Nothing
                    )
                |> List.filterMap identity
    in
    if List.length matches == countRelevant buses then
        t

    else
        scheduleAtTime (t + List.product matches) buses


countRelevant : List Bus -> Int
countRelevant =
    List.map
        (\b ->
            if b == Irrelevant then
                0

            else
                1
        )
        >> List.sum


parseInput : String -> ( Int, List Bus )
parseInput lines =
    case lines |> String.split "\u{000D}\n" of
        [ t, buses ] ->
            ( String.toInt t |> Maybe.withDefault 0
            , buses
                |> String.split ","
                |> List.map
                    (String.toInt
                        >> Maybe.map Relevant
                        >> Maybe.withDefault Irrelevant
                    )
            )

        _ ->
            ( 0, [] )


output : String -> Html msg
output s =
    div [] [ text s ]


sample : String
sample =
    """939
7,13,x,x,59,x,31,19"""


input : String
input =
    """1006605
19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,883,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,x,x,797,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29"""
