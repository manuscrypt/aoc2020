module Main exposing (main)

import Html exposing (Html, div, text)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, getChompedString, int, keyword, run, spaces, symbol)


type alias Note =
    { category : String
    , rangeA : ( Int, Int )
    , rangeB : ( Int, Int )
    }


type alias Ticket =
    List Int


main : Html.Html msg
main =
    let
        ( notes, tickets ) =
            ( parseNotes notesString, parseNearbyTickets nearbyTicketsString )

        partA =
            tickets
                |> List.map (invalidNums notes)
                |> List.concat
                |> List.sum

        partB =
            (myTicket :: validTickets notes tickets)
                |> classifyNotes notes
                |> collect []
                |> departureNotes
                |> List.map Tuple.first
                |> List.map (\i -> List.getAt i myTicket |> Maybe.withDefault 0)
                |> List.product
    in
    div []
        [ output "part1"
        , output (String.fromInt partA)
        , output "part2"
        , output (String.fromInt partB)
        ]


departureNotes : List ( Int, Note ) -> List ( Int, Note )
departureNotes =
    List.filter (\( _, n ) -> String.startsWith "departure" n.category)


collect : List ( Int, Note ) -> List ( Int, List Note ) -> List ( Int, Note )
collect result list =
    if List.length list == 0 then
        result

    else
        case list |> List.filter (Tuple.second >> List.length >> (==) 1) of
            [ x ] ->
                case x of
                    ( idx, [ theNote ] ) ->
                        let
                            remaining =
                                list
                                    |> List.map
                                        (Tuple.mapSecond
                                            (List.filter (\n -> n /= theNote))
                                        )
                                    |> List.filter
                                        (Tuple.second
                                            >> List.length
                                            >> (/=) 0
                                        )
                        in
                        collect (( idx, theNote ) :: result) remaining

                    _ ->
                        Debug.todo "cant do it 2"

            _ ->
                Debug.todo "cant do it 1"


classifyNotes : List Note -> List Ticket -> List ( Int, List Note )
classifyNotes notes tickets =
    List.range 0 (List.length notes - 1)
        |> List.map
            (\i ->
                ( i
                , tickets
                    |> List.map (List.getAt i >> Maybe.withDefault -1)
                )
            )
        |> List.map
            (\( idx, ticket ) ->
                ( idx
                , notes
                    |> List.filter (allValidOnNote ticket)
                )
            )


validTickets : List Note -> List Ticket -> List Ticket
validTickets notes =
    List.filter (invalidNums notes >> List.length >> (==) 0)


invalidNums : List Note -> Ticket -> List Int
invalidNums notes =
    List.filter (\num -> not <| isValidOnAnyNote num notes)


isValidOnAnyNote : Int -> List Note -> Bool
isValidOnAnyNote num =
    List.any (\n -> isValidOnNote n num)


allValidOnNote : Ticket -> Note -> Bool
allValidOnNote ticket n =
    List.all (isValidOnNote n) ticket


isValidOnNote : Note -> Int -> Bool
isValidOnNote n val =
    isValidInRange n.rangeA val
        || isValidInRange n.rangeB val


isValidInRange : ( Int, Int ) -> Int -> Bool
isValidInRange ( lo, hi ) val =
    val >= lo && val <= hi


parseNearbyTickets : String -> List Ticket
parseNearbyTickets =
    String.lines
        >> List.map
            (String.split ","
                >> List.filterMap String.toInt
            )


parseNotes : String -> List Note
parseNotes =
    String.lines
        >> List.filterMap
            (Parser.run noteParser
                >> Result.toMaybe
            )


output : String -> Html msg
output s =
    div [] [ text s ]


noteParser : Parser Note
noteParser =
    Parser.succeed Note
        |= (Parser.chompUntil ":" |> getChompedString)
        |. symbol ":"
        |. spaces
        |= rangeParser
        |. spaces
        |. keyword "or"
        |. spaces
        |= rangeParser


rangeParser : Parser ( Int, Int )
rangeParser =
    Parser.succeed Tuple.pair
        |= int
        |. symbol "-"
        |= int


myTicket : Ticket
myTicket =
    [ 223, 139, 211, 131, 113, 197, 151, 193, 127, 53, 89, 167, 227, 79, 163, 199, 191, 83, 137, 149 ]


notesString : String
notesString =
    """departure location: 39-715 or 734-949
departure station: 30-152 or 160-959
departure platform: 34-780 or 798-955
departure track: 32-674 or 699-952
departure date: 38-55 or 74-952
departure time: 45-533 or 547-970
arrival location: 27-168 or 191-969
arrival station: 43-585 or 599-953
arrival platform: 40-831 or 837-961
arrival track: 37-293 or 301-974
class: 40-89 or 112-950
duration: 25-600 or 625-970
price: 45-318 or 341-954
route: 40-898 or 912-968
row: 38-872 or 881-958
seat: 37-821 or 831-958
train: 26-343 or 365-956
type: 37-857 or 872-960
wagon: 36-425 or 445-972
zone: 44-270 or 286-967"""


nearbyTicketsString : String
nearbyTicketsString =
    """649,669,116,341,816,410,776,496,725,647,853,163,397,449,557,554,492,523,384,526
818,472,885,771,224,139,250,511,670,784,940,142,584,891,646,889,651,311,83,747
374,576,228,459,86,901,648,497,486,509,496,500,270,524,668,399,702,114,370,455
706,50,769,854,498,477,651,265,890,256,160,279,777,518,463,776,872,737,312,809
184,706,920,238,55,625,55,215,752,584,398,379,767,762,197,890,526,700,576,490
495,392,211,801,137,753,658,575,663,281,112,126,599,661,222,710,667,566,942,51
86,551,296,745,653,846,478,572,388,671,517,770,660,521,642,289,120,837,406,925
122,240,150,521,218,614,409,672,210,372,473,216,565,468,160,669,140,848,134,470
215,915,217,85,407,497,815,367,798,949,527,167,712,824,250,768,819,460,703,641
491,842,666,315,919,499,479,503,820,464,856,244,916,996,408,929,770,948,146,88
914,420,726,890,454,699,293,258,420,516,525,817,839,391,818,492,222,139,449,801
574,282,223,668,509,943,499,164,948,525,508,643,365,229,445,389,450,563,760,421
160,762,588,756,811,522,842,924,668,405,750,380,701,209,205,375,265,756,574,267
805,669,629,244,938,597,884,456,302,260,851,218,634,575,919,758,304,804,937,579
654,502,663,535,138,582,734,946,779,85,815,670,516,734,149,490,306,86,740,161
777,201,249,470,947,69,941,245,805,380,570,128,549,449,926,933,378,249,674,269
267,390,452,205,231,454,490,269,906,232,415,223,121,653,840,193,488,942,577,238
652,454,487,141,138,251,705,371,926,936,265,526,701,572,630,664,302,602,467,626
743,656,219,566,667,183,420,246,421,513,559,76,120,674,767,816,949,661,736,811
235,126,416,259,507,51,655,234,882,806,483,762,478,609,894,701,392,668,919,407
656,89,773,763,754,529,665,654,924,209,568,229,893,896,225,666,714,258,902,468
779,415,372,248,719,501,531,564,577,386,288,224,654,286,661,401,912,581,759,117
752,86,243,469,82,315,857,883,818,599,888,937,231,413,733,571,799,666,140,498
85,767,147,933,649,653,143,777,80,214,713,577,995,266,410,377,195,54,756,380
66,489,479,301,582,883,815,658,81,654,745,779,738,848,123,54,780,772,660,379
820,167,244,857,203,250,198,461,818,936,23,120,578,136,80,379,775,666,627,311
244,475,204,259,294,814,79,915,527,660,142,711,893,343,259,806,772,917,850,773
673,234,82,145,408,393,597,192,384,53,452,305,768,459,772,798,742,840,818,143
563,211,664,656,751,18,627,138,470,922,819,316,86,408,513,547,86,227,715,471
577,288,388,786,264,629,376,315,74,872,450,139,526,626,310,51,631,946,129,569
915,781,707,262,248,890,54,671,661,659,248,53,842,424,564,769,626,201,629,368
822,851,765,168,412,578,268,77,378,912,932,133,849,150,872,382,580,842,934,569
164,483,365,468,994,710,509,86,135,558,218,424,266,259,937,760,493,138,566,553
491,940,143,237,51,668,304,390,369,265,580,920,525,56,221,949,701,779,759,130
217,83,240,135,212,473,197,368,74,810,234,501,200,220,449,364,126,578,670,527
939,586,630,249,941,928,210,851,756,150,398,380,214,937,80,146,526,896,810,492
246,648,129,886,769,520,280,412,480,945,460,502,913,884,489,815,234,240,814,777
753,776,308,715,514,303,481,523,914,132,801,404,932,897,378,130,662,358,412,574
896,400,581,201,476,465,552,765,577,199,233,257,512,509,551,630,930,233,982,134
311,942,372,886,471,398,629,920,241,307,290,820,560,465,295,559,737,397,512,922
335,135,577,502,737,886,752,77,531,208,707,479,211,705,641,512,401,251,55,223
750,775,196,357,52,313,499,471,314,148,657,556,203,193,883,238,776,715,803,506
450,872,934,870,453,579,881,529,412,674,125,485,918,526,460,562,508,200,773,81
449,765,816,719,918,770,748,567,113,304,654,857,454,932,558,219,74,114,599,831
455,77,156,82,267,290,564,625,202,269,161,657,262,634,509,850,501,389,519,714
586,413,113,194,750,891,499,255,451,231,927,141,798,704,531,131,414,745,511,211
694,266,226,468,374,490,699,944,302,921,530,743,946,459,486,671,168,88,483,240
672,132,516,453,459,891,919,598,139,895,390,402,416,227,573,640,230,893,851,199
402,255,539,310,521,305,262,367,201,377,847,53,515,413,388,477,922,847,708,221
788,652,343,255,259,286,292,164,831,151,387,752,508,577,231,292,634,523,117,237
553,708,651,674,492,809,645,373,209,502,460,930,993,365,268,491,657,774,411,207
709,268,126,856,896,417,408,930,921,385,404,343,454,268,259,590,454,512,200,943
806,704,149,516,566,481,139,849,886,86,560,846,76,875,379,846,226,141,758,764
315,476,744,856,850,530,196,412,618,838,578,921,771,454,206,564,316,817,412,251
647,641,191,458,925,736,584,453,408,533,152,513,5,76,405,126,646,84,120,925
452,373,936,763,583,120,903,548,844,219,886,293,740,518,897,523,192,494,883,520
426,203,667,742,231,948,205,168,217,652,625,776,264,755,923,445,523,488,516,397
258,250,496,221,888,705,248,653,738,221,889,518,767,895,840,568,742,922,765,21
707,158,369,635,734,749,132,740,780,255,292,291,843,229,270,831,526,881,162,743
150,341,643,130,285,671,191,217,939,632,933,287,580,239,318,526,486,257,379,773
449,884,456,771,487,932,519,409,850,712,291,394,204,414,710,423,132,709,363,222
577,134,409,631,376,451,914,398,708,521,294,253,912,308,808,581,913,918,821,654
873,127,946,562,573,382,759,499,914,890,343,310,765,396,742,76,54,921,458,117
728,315,495,464,475,666,945,381,402,212,844,548,118,566,514,762,759,813,518,508
398,262,854,884,920,244,142,613,751,505,939,487,808,578,500,139,223,499,646,635
855,506,506,218,396,936,660,699,884,417,419,160,771,552,122,984,406,466,419,532
243,529,687,120,599,771,634,195,515,317,498,290,930,413,399,655,890,629,758,375
400,122,412,643,808,536,919,301,244,855,577,571,377,643,557,762,213,762,82,142
670,774,701,247,569,481,420,255,494,471,946,892,75,424,748,370,772,240,276,632
471,765,69,398,643,762,461,231,386,849,288,915,82,652,80,820,666,843,192,891
50,712,262,988,801,392,424,121,891,226,368,657,77,144,581,651,446,814,737,568
287,906,292,150,505,912,144,760,630,560,929,477,774,194,381,389,658,381,495,912
669,129,318,139,801,495,221,402,933,747,201,244,403,870,523,633,233,803,504,893
132,128,121,949,260,460,776,502,526,146,306,774,471,203,519,888,741,133,483,977
211,702,523,249,934,366,367,482,316,140,538,453,88,193,197,467,479,652,375,146
755,76,649,484,926,811,447,851,707,152,569,234,132,798,85,910,853,754,420,452
549,196,118,469,715,635,764,82,560,54,8,86,192,315,890,626,74,396,466,647
762,571,275,227,746,505,664,395,253,663,810,767,810,388,821,374,494,128,343,638
143,943,466,394,467,528,739,934,454,634,699,458,571,176,775,77,462,245,819,208
414,846,774,455,139,471,674,805,821,224,86,584,113,11,493,161,637,422,318,744
234,490,796,131,312,199,650,311,308,251,141,629,407,548,120,645,817,249,290,661
476,212,672,182,502,642,494,192,667,916,699,848,317,945,927,236,637,527,163,475
503,550,713,638,147,384,831,483,260,843,741,166,51,246,581,254,604,75,168,738
916,577,422,758,454,357,266,214,413,368,414,262,240,887,644,637,645,467,800,559
636,74,940,517,144,460,474,471,399,155,524,458,253,373,915,259,887,424,669,741
924,578,559,55,933,219,85,751,239,468,287,707,583,549,314,299,918,447,897,392
626,303,800,854,513,628,166,913,918,654,201,368,712,821,928,213,15,53,116,287
527,76,162,257,75,226,456,656,462,546,949,457,52,288,649,256,393,627,142,390
393,363,167,472,309,761,234,629,485,465,311,367,930,193,799,844,226,579,635,646
900,737,245,548,630,412,293,524,919,461,461,264,799,303,758,744,76,800,127,391
525,394,599,884,760,700,215,941,482,851,249,740,501,723,945,217,627,375,746,236
163,85,317,758,699,53,772,937,893,497,901,558,497,563,314,671,366,200,584,408
628,140,258,261,976,915,310,215,849,393,416,376,390,947,210,549,460,311,228,225
236,220,648,463,736,126,711,485,400,261,457,625,418,644,350,480,208,139,751,391
84,578,920,196,195,406,224,119,396,183,138,759,912,850,778,408,762,922,385,450
139,809,866,856,196,885,800,518,204,144,228,249,942,263,270,122,480,418,576,740
459,112,920,148,575,837,407,83,990,928,573,756,225,497,223,390,239,776,124,461
892,933,481,63,127,131,768,252,510,655,149,477,713,626,770,228,800,138,779,468
494,555,396,664,588,763,709,144,240,806,236,652,478,519,839,844,740,456,485,267
292,89,255,252,843,205,229,445,260,241,625,658,505,280,555,703,248,473,74,265
309,531,244,749,908,799,233,671,220,77,657,766,148,570,405,643,488,600,203,306
318,809,674,483,249,628,737,753,416,160,926,390,894,625,885,975,479,933,266,74
261,228,318,886,938,210,602,872,261,401,425,387,932,402,850,405,937,652,241,801
847,702,754,260,414,948,287,997,224,528,290,894,261,152,487,843,89,268,407,831
214,734,551,880,128,761,516,312,420,760,404,137,52,756,840,764,247,701,289,738
415,401,232,124,231,930,314,161,265,492,563,764,122,167,1,914,137,399,857,152
131,471,119,651,207,248,373,919,857,911,702,289,506,803,217,249,123,410,673,533
266,401,562,162,618,88,415,315,419,638,409,752,775,371,840,854,557,151,703,133
509,74,143,552,456,656,519,652,417,315,148,242,855,153,205,629,195,755,113,161
114,211,736,79,563,764,446,841,467,215,847,857,403,444,457,240,574,200,889,246
653,737,140,467,709,560,564,3,582,394,516,365,886,223,652,648,564,449,669,661
941,411,137,854,376,449,738,917,525,377,277,568,468,452,654,394,856,660,744,552
930,229,132,947,811,450,478,114,367,201,378,473,887,127,831,643,479,533,5,750
55,705,501,261,495,165,314,268,783,699,840,812,837,460,922,404,667,368,123,914
529,566,383,657,488,461,292,122,763,511,568,311,255,336,658,779,523,655,89,237
468,305,944,803,920,464,202,146,654,476,551,616,633,838,123,236,117,389,520,507
84,421,494,947,629,143,650,372,647,525,920,936,810,476,462,673,205,265,723,707
250,562,451,416,116,455,214,943,337,653,565,918,126,576,121,644,400,847,267,134
515,662,552,137,528,302,669,780,242,898,604,202,640,208,780,517,849,480,739,628
578,736,848,468,577,124,513,306,799,771,404,570,248,524,384,122,738,445,534,926
559,368,264,150,411,214,117,260,475,644,748,242,80,259,663,240,581,714,126,977
191,802,838,371,194,782,205,808,669,484,582,86,305,771,777,292,701,799,233,241
113,474,938,549,676,375,286,839,515,370,386,292,661,938,533,653,526,255,466,664
293,386,462,160,379,890,895,242,243,912,384,140,448,201,598,937,810,840,646,640
635,622,755,778,571,257,377,579,414,758,196,113,200,405,585,82,645,500,480,662
380,119,198,560,939,850,460,459,371,496,477,78,199,921,248,611,497,392,948,561
262,895,644,881,314,699,584,468,700,580,912,373,209,448,749,580,918,558,461,994
375,712,744,674,661,480,483,372,702,714,195,744,214,677,293,921,891,341,633,748
255,580,20,79,882,456,752,217,497,504,704,75,194,819,486,380,264,530,502,739
768,420,135,209,572,892,927,299,414,882,302,216,809,267,495,892,130,660,815,841
662,54,808,392,234,150,316,637,311,668,218,224,882,415,985,424,915,856,467,132
550,131,82,490,483,461,817,757,639,66,849,262,384,385,633,260,498,500,114,480
296,372,457,918,932,579,220,647,800,166,920,245,505,651,567,884,451,529,515,919
102,523,388,488,223,194,493,401,88,800,201,663,374,527,116,889,715,402,488,207
570,202,168,615,505,134,556,632,251,290,855,763,741,259,201,137,701,803,470,937
418,522,897,678,381,114,655,389,416,775,561,470,801,204,817,412,82,205,228,842
775,85,707,944,814,934,552,800,52,358,896,744,753,130,812,523,889,771,925,79
520,664,637,143,302,640,129,419,672,846,744,469,894,417,843,278,845,628,237,930
247,764,767,946,487,778,264,703,702,696,578,758,913,669,208,239,372,627,584,897
627,213,645,226,517,195,462,493,715,553,739,939,748,537,77,260,138,739,743,585
464,151,160,715,193,548,20,234,260,518,711,454,484,485,767,919,483,817,944,252
165,82,227,941,64,204,510,672,638,493,391,258,76,804,116,220,931,890,318,707
531,465,772,198,288,707,649,196,948,81,533,737,760,495,664,572,506,317,68,452
612,127,509,915,151,671,847,924,485,220,888,847,518,657,810,517,487,120,244,293
457,739,925,703,380,468,117,369,767,229,344,404,936,246,142,313,768,558,821,368
408,146,19,124,746,454,525,164,742,573,913,707,843,318,86,218,259,925,625,149
631,990,751,554,494,947,259,775,886,247,308,494,803,769,940,515,669,743,130,779
395,466,86,152,530,500,558,136,850,312,566,294,518,570,128,518,748,209,471,251
519,165,881,564,195,863,914,219,409,305,51,142,87,579,813,51,701,584,927,151
633,255,404,52,208,675,288,646,627,626,342,655,734,501,229,703,424,133,229,167
546,261,234,778,518,746,454,484,670,367,250,764,522,454,817,225,133,915,453,55
699,66,510,397,304,197,638,246,82,315,397,819,706,804,292,735,138,775,774,914
557,225,765,366,893,713,501,483,853,942,943,642,629,783,672,286,938,451,559,302
640,658,135,234,812,317,560,83,196,450,55,665,849,143,202,896,743,895,776,985
461,117,584,774,306,844,310,83,807,606,777,239,372,127,770,817,460,487,238,450
712,399,476,560,556,384,150,424,551,203,648,849,215,702,117,469,6,89,51,551
478,296,459,843,585,778,125,501,844,740,839,810,269,762,522,600,848,626,628,488
448,700,194,636,160,312,4,214,52,385,497,857,570,221,916,887,488,520,387,644
387,462,654,775,515,202,932,449,136,530,218,467,637,200,316,216,143,538,461,457
468,390,82,159,410,400,872,754,766,766,625,920,245,632,473,119,943,516,935,341
149,816,366,817,529,521,566,669,840,747,747,886,225,524,540,135,162,380,504,213
937,496,906,850,804,645,547,121,460,89,138,303,816,234,312,842,741,445,841,580
631,890,738,266,585,77,480,493,244,445,799,15,510,457,522,207,529,633,802,387
501,737,204,852,345,342,421,54,818,530,747,417,818,766,312,451,883,251,674,556
220,633,471,376,376,811,296,714,666,217,831,497,547,649,703,248,745,255,478,699
552,885,552,772,467,700,923,944,298,521,74,938,641,213,227,658,288,240,920,916
570,582,483,51,912,483,807,629,633,569,588,776,631,752,248,485,943,700,845,116
849,706,662,890,420,276,312,483,518,918,930,123,310,709,492,666,166,196,553,218
654,885,411,930,472,161,853,800,753,897,203,931,489,222,470,424,53,391,588,774
266,87,487,528,571,525,88,126,598,654,857,476,232,636,425,503,473,777,734,937
477,562,388,53,497,158,128,811,76,774,921,161,556,82,671,461,820,393,579,752
159,945,514,423,386,808,507,235,416,930,709,116,844,892,820,291,505,855,853,744
360,366,517,474,288,242,567,918,555,755,142,532,753,503,853,642,642,578,702,809
748,760,656,671,890,947,417,987,468,265,81,84,892,258,580,251,521,626,751,406
292,166,751,217,448,161,753,749,848,872,499,450,531,853,462,450,570,774,691,161
516,856,451,306,912,321,483,498,819,562,318,468,54,773,77,927,267,659,849,935
894,288,805,512,673,377,502,772,629,748,127,195,469,424,267,390,294,652,54,750
548,391,211,599,544,525,233,663,511,304,201,408,769,399,490,116,599,642,53,750
287,117,895,516,303,482,74,894,281,243,415,448,886,209,556,490,452,129,387,816
736,585,561,199,570,447,219,405,575,725,78,659,670,342,253,126,651,746,253,262
496,703,248,167,883,888,469,398,473,574,925,135,386,199,56,914,886,935,221,658
844,7,127,222,128,708,205,125,468,191,664,217,518,118,191,388,399,896,234,803
915,247,238,137,522,648,485,388,239,533,113,851,218,669,453,163,126,849,604,748
276,205,574,467,217,734,704,263,710,941,841,504,926,627,745,311,452,415,80,52
457,541,117,149,165,857,811,144,734,213,504,662,368,517,844,167,462,547,671,931
213,754,406,483,599,916,530,390,803,307,775,318,671,389,51,757,462,282,930,236
558,248,113,194,573,113,569,12,476,259,373,804,774,221,372,919,477,661,936,529
665,855,743,303,375,941,761,839,700,556,68,765,763,527,631,82,124,811,464,884
496,747,397,328,882,761,231,253,759,763,253,341,144,942,424,265,645,565,286,533
245,845,229,814,151,301,817,947,737,925,775,597,801,843,230,77,420,713,745,448
417,680,886,255,53,562,634,548,643,499,550,929,511,115,923,561,124,79,523,734
233,202,813,568,161,880,657,550,672,51,196,556,943,366,144,566,513,395,289,776
505,527,118,495,199,524,390,201,870,657,415,772,919,527,747,215,772,208,946,887
805,991,226,883,142,136,342,896,50,257,407,667,452,228,402,231,811,943,555,626
557,532,780,549,121,238,926,425,62,419,744,424,714,302,379,237,233,231,561,202
252,404,392,809,769,227,409,812,374,851,164,136,646,941,114,499,417,588,752,211
244,89,652,928,574,938,374,747,945,195,580,235,776,900,418,405,526,200,641,74
938,640,255,283,633,627,525,526,141,289,243,52,751,503,766,403,128,122,486,145
0,659,230,715,55,849,894,243,473,670,315,220,396,314,937,888,510,403,201,230
757,168,456,495,301,770,938,479,462,334,881,78,454,700,228,302,814,501,762,811
255,262,752,948,699,496,773,492,384,587,920,387,482,669,50,86,132,666,930,837
193,317,561,852,977,250,780,745,599,775,804,506,422,640,648,399,499,638,847,376
204,851,549,554,896,296,802,628,843,708,87,233,639,377,779,409,469,897,243,218
132,728,496,663,140,209,135,455,257,203,930,852,228,580,86,149,253,77,929,514
403,471,757,77,465,233,804,658,355,512,630,119,125,714,579,746,228,122,142,750
448,514,211,841,138,395,287,142,672,292,705,581,459,53,624,569,530,149,845,552
580,52,798,289,748,341,582,566,121,863,229,493,741,579,342,253,893,648,576,775
600,53,912,382,115,730,414,912,757,556,933,267,75,659,817,779,387,765,773,650
404,145,582,742,192,123,395,884,224,409,853,449,116,363,891,266,554,475,580,574
400,167,516,903,560,468,554,584,557,654,667,478,703,80,286,918,809,147,414,133
754,135,13,843,659,625,472,124,585,510,217,767,246,249,916,748,762,856,497,771
805,520,490,267,140,820,918,391,159,160,164,54,87,287,480,808,412,559,148,195
714,672,650,709,211,748,449,208,522,75,810,198,944,643,280,657,463,195,457,946
703,135,700,376,507,772,208,527,537,669,739,463,138,453,248,471,313,291,77,129
893,743,552,630,521,141,254,490,661,583,215,342,78,210,906,79,382,50,112,167
87,122,246,395,510,244,391,507,990,484,801,775,232,289,406,219,308,126,928,770
706,236,932,478,935,119,132,630,577,342,470,839,807,490,515,461,933,886,794,205
857,657,19,944,137,406,934,365,771,701,194,119,458,201,454,123,657,575,750,813
402,773,511,550,259,449,739,254,381,54,765,574,149,702,928,845,514,907,75,847
662,516,637,52,599,764,845,705,466,298,920,415,257,551,244,517,504,504,406,501
819,399,850,844,133,811,216,585,87,394,312,418,930,856,660,383,816,294,261,748
218,560,609,514,665,394,124,371,489,480,664,168,127,814,412,892,144,746,265,939
868,122,809,241,709,563,739,152,465,814,447,481,302,774,451,803,628,393,882,410
341,51,395,245,938,256,764,203,500,947,573,643,461,215,244,3,646,470,511,396
259,664,349,661,50,926,857,229,574,556,454,887,803,374,576,881,391,425,420,770
857,884,404,54,481,84,926,804,898,263,668,492,471,300,121,811,755,600,746,486
663,241,371,245,242,460,483,749,164,211,561,975,118,547,476,754,115,560,292,401
230,393,460,296,416,779,88,398,198,524,888,201,815,116,734,798,533,563,407,423
396,886,112,750,477,434,747,919,887,461,233,82,248,234,754,499,367,466,451,848
468,579,365,699,743,872,944,802,676,226,854,762,774,480,813,318,76,775,807,287
252,383,54,592,948,472,238,739,772,228,113,633,752,316,226,712,87,400,771,817
762,631,921,583,204,882,198,845,812,531,571,204,470,590,253,313,504,633,515,144
407,505,309,778,653,773,401,755,661,234,889,196,199,626,514,395,459,576,629,603
887,745,200,556,502,915,291,857,152,419,573,843,924,227,256,376,667,512,297,222
239,213,529,447,384,482,381,192,512,135,927,10,304,138,213,817,86,699,120,912"""
