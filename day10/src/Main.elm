module Main exposing (main)

import Array
import Html exposing (Html, div, text)
import List
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, keyword, oneOf, succeed)
import Tree exposing (Tree)


main : Html.Html msg
main =
    let
        nums =
            parseInput sampleA |> Debug.log "a"

        partA =
            solveA nums

        partB =
            solveB nums 0 []
                |> Debug.log "tree"
                |> List.length
    in
    div []
        [ output "part1"
        , output (String.fromInt partA)
        , output "part2"
        , output (String.fromInt partB)
        ]


solveB : List Int -> Int -> List (List Int) -> List (List Int)
solveB nums index lists =
    case List.getAt index nums of
        Nothing ->
            lists

        Just c ->
            let
                xx =
                    Debug.log "c" c

                validPaths =
                    List.filter (\i -> i > c && i - c <= 3) nums
                        |> Debug.log "vps"
            in
            solveB nums (index + 1) (lists ++ [ validPaths ])


getValid : List Int -> List (List Int)
getValid nums =
    List.range 1 (List.length nums - 1)
        |> List.map (\i -> List.removeAt i nums)
        |> List.filter isValid


diffs2 : List Int -> List Int
diffs2 nums =
    List.zip (List.drop 1 nums) nums
        |> List.map (\( a, b ) -> abs (a - b))


solveA : List Int -> Int
solveA j1 =
    diffs2 j1
        |> List.gatherEquals
        |> List.filter (\i -> Tuple.first i /= 2)
        |> List.map
            (\( _, l ) ->
                1 + List.length l
            )
        |> List.product


isValid : List Int -> Bool
isValid nums =
    if List.length nums == 0 then
        False

    else
        List.all (\i -> i < 4) (diffs2 nums)


output : String -> Html msg
output s =
    div [] [ text s ]



--input


withExtraElements : List Int -> List Int
withExtraElements nums =
    let
        last =
            nums |> List.maximum |> Maybe.withDefault 100000 |> (+) 3
    in
    (0 :: nums) ++ [ last ]


parseInput : String -> List Int
parseInput str =
    String.lines str
        |> List.filterMap String.toInt
        |> List.sort
        |> withExtraElements


mySample : String
mySample =
    """1
3
6
7"""


sampleA : String
sampleA =
    """16
10
15
5
1
11
7
19
6
12
4"""


sampleB : String
sampleB =
    """28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"""


input : String
input =
    """74
153
60
163
112
151
22
67
43
160
193
6
2
16
122
126
32
181
180
139
20
111
66
81
12
56
63
95
90
161
33
134
31
119
53
148
104
91
140
36
144
23
130
178
146
38
133
192
131
3
73
11
62
50
89
98
103
110
164
48
80
179
92
194
86
40
13
123
68
115
19
46
77
152
138
69
49
59
30
132
9
185
1
188
171
72
116
101
61
141
107
21
47
147
182
170
39
37
127
26
102
137
191
162
172
29
10
154
157
83
82
175
145
167"""
