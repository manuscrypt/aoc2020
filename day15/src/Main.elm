module Main exposing (main)

import Html exposing (Html, div, text)
import List.Extra as List


type alias Model =
    { index : Int
    , prev : Int
    , history : List ( Int, Int )
    }


main : Html.Html msg
main =
    let
        localInput =
            input

        partA =
            solveTarget 30000000 (parseInput localInput)

        partB =
            -1

        --solveTarget 30000000 (parseInput localInput)
        -- xx =
        --     solveUpTo 2020 localInput
        --solveTarget 100 (parseInput localInput)
    in
    div []
        [ output "part1"
        , output (String.fromInt partA)
        , output "part2"
        , output (String.fromInt partB)
        ]


solveUpTo : Int -> String -> List (List Int)
solveUpTo count str =
    List.range 0 count
        |> List.map
            (\c ->
                solveA c (parseInput str)
                    |> .history
                    |> List.map Tuple.second
                    |> (\h -> Debug.log (String.fromInt c ++ "=>" ++ String.fromInt (List.length h)) h)
            )


solveTarget : Int -> Model -> Int
solveTarget target model =
    let
        result =
            solveA target model
    in
    List.head result.history
        |> Maybe.map Tuple.second
        |> Maybe.withDefault -1


solveA : Int -> Model -> Model
solveA target model =
    if model.index == target then
        model

    else
        let
            matches =
                model.history
                    |> List.filter (\( _, n ) -> n == model.prev)

            next =
                case matches of
                    [ _ ] ->
                        { model
                            | history = ( model.index + 1, 0 ) :: model.history
                            , prev = 0
                            , index = model.index + 1
                        }

                    [ x, y ] ->
                        let
                            diff =
                                abs (Tuple.first y - Tuple.first x)

                            newHistory =
                                model.history
                                    |> List.filter (\t -> t /= y)
                        in
                        { model
                            | history = ( model.index + 1, diff ) :: newHistory
                            , prev = diff
                            , index = model.index + 1
                        }

                    _ ->
                        model

            -- yy =
            --     Debug.log "spoken" next.prev
        in
        solveA target next


parseInput : String -> Model
parseInput line =
    let
        nums =
            String.split "," line
                |> List.filterMap String.toInt
    in
    { index = List.length nums
    , prev = nums |> List.reverse |> List.head |> Maybe.withDefault -1
    , history = nums |> List.indexedMap (\i n -> ( i + 1, n )) |> List.reverse
    }


output : String -> Html msg
output s =
    div [] [ text s ]


sampleA : String
sampleA =
    """0,3,6"""


sampleB : String
sampleB =
    """1,3,2"""


sampleC : String
sampleC =
    """2,1,3"""


input : String
input =
    """13,16,0,12,15,1"""
