module Main exposing (main)

import Array exposing (Array)
import Binary
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Parser exposing ((|.), (|=), getChompedString, int, keyword, spaces, succeed, symbol)
import Parser.Advanced exposing (chompUntilEndOr)


type MaskBit
    = X
    | Zero
    | One


type alias Mask =
    List MaskBit


type Instruction
    = UpdateMask Mask
    | WriteValue Int Int


type alias Model =
    { pc : Int
    , mask : Mask
    , instructions : Array Instruction
    , memory : Dict Int Int
    }


main : Html.Html msg
main =
    let
        model =
            parseInput input

        partA =
            applyNextInstruction model
                |> .memory
                |> Dict.values
                |> List.sum

        partB =
            applyNextDecoder model
                |> .memory
                |> Dict.values
                |> List.sum
    in
    div []
        [ output "part1"
        , output (String.fromInt partA)
        , output "part2"
        , output (String.fromInt partB)
        ]


applyNextDecoder : Model -> Model
applyNextDecoder model =
    case Array.get model.pc model.instructions of
        Nothing ->
            model

        Just i ->
            case i of
                UpdateMask mask ->
                    { model
                        | mask = mask
                        , pc = model.pc + 1
                    }

                WriteValue addr val ->
                    { model
                        | memory =
                            getAddresses model.mask addr
                                |> List.foldl
                                    (\a mem ->
                                        Dict.insert a val mem
                                    )
                                    model.memory
                        , pc = model.pc + 1
                    }
                        |> applyNextDecoder


getAddresses : Mask -> Int -> List Int
getAddresses mask val =
    let
        bits =
            val
                |> Binary.fromDecimal
                |> Binary.ensureSize (List.length mask)
                |> Binary.toIntegers
    in
    List.map2
        (\b m ->
            case m of
                Zero ->
                    if b == 0 then
                        Zero

                    else
                        One

                One ->
                    One

                X ->
                    X
        )
        bits
        mask
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( i, b ) all ->
                case b of
                    Zero ->
                        all |> List.map (Array.set i 0)

                    One ->
                        all |> List.map (Array.set i 1)

                    X ->
                        all
                            |> List.map
                                (\a ->
                                    [ Array.set i 0 a, Array.set i 1 a ]
                                )
                            |> List.concat
            )
            [ List.repeat 36 0 |> Array.fromList ]
        |> List.map
            (Array.toList
                >> Binary.fromIntegers
                >> Binary.toDecimal
            )


applyNextInstruction : Model -> Model
applyNextInstruction model =
    case Array.get model.pc model.instructions of
        Nothing ->
            model

        Just i ->
            case i of
                UpdateMask mask ->
                    { model | mask = mask, pc = model.pc + 1 }

                WriteValue addr val ->
                    { model
                        | memory =
                            Dict.insert addr
                                (applyMask model.mask val)
                                model.memory
                        , pc = model.pc + 1
                    }
                        |> applyNextInstruction


applyMask : List MaskBit -> Int -> Int
applyMask arr val =
    let
        bits =
            val
                |> Binary.fromDecimal
                |> Binary.ensureSize (List.length arr)
                |> Binary.toIntegers
    in
    List.map2
        (\b m ->
            case m of
                X ->
                    b

                Zero ->
                    0

                One ->
                    1
        )
        bits
        arr
        |> Binary.fromIntegers
        |> Binary.toDecimal


parseInput : String -> Model
parseInput lines =
    { pc = 0
    , memory = Dict.empty
    , mask = List.repeat 36 Zero
    , instructions =
        lines
            |> String.lines
            |> List.filterMap (Parser.run instruction >> Result.toMaybe)
            |> Array.fromList
    }


parseMask : String -> List MaskBit
parseMask s =
    String.toList s
        |> List.filterMap
            (\c ->
                case c of
                    'X' ->
                        Just X

                    '1' ->
                        Just One

                    '0' ->
                        Just Zero

                    _ ->
                        Nothing
            )


instruction : Parser.Parser Instruction
instruction =
    Parser.oneOf [ updateMask, writeValue ]


updateMask : Parser.Parser Instruction
updateMask =
    succeed UpdateMask
        |. keyword "mask"
        |. spaces
        |. symbol "="
        |. spaces
        |= Parser.map parseMask (chompUntilEndOr "\n" |> getChompedString)


writeValue : Parser.Parser Instruction
writeValue =
    succeed WriteValue
        |. keyword "mem"
        |. symbol "["
        |= int
        |. symbol "]"
        |. spaces
        |. symbol "="
        |. spaces
        |= int


output : String -> Html msg
output s =
    div [] [ text s ]


sampleA : String
sampleA =
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"""


sampleB : String
sampleB =
    """mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"""


input : String
input =
    """mask = 001X11X1X010X1X1010XX10X100101011000
mem[43398] = 563312
mem[51673] = 263978
mem[18028] = 544304215
mask = X0100001101XX11100010XX110XX11111000
mem[24151] = 2013
mem[15368] = 19793
mem[45005] = 478
mem[1842] = 190808161
mem[36033] = 987
mem[26874] = 102
mask = 00X0000110110X000110010101XX0X010001
mem[9507] = 7
mem[50019] = 16475608
mem[4334] = 129799
mem[37373] = 182640
mem[28170] = 534617265
mem[6432] = 354252
mem[36752] = 834628
mask = 10100000101101100110X001X0X001100X10
mem[36664] = 30481
mem[6532] = 103013119
mem[45659] = 15629
mem[19533] = 167227
mem[40461] = 344193233
mem[6217] = 26713310
mask = X0XX010100110101X0001101X11100X100X0
mem[38530] = 6202
mem[53032] = 13775
mem[39333] = 1003152
mem[3932] = 1240562
mem[59246] = 12638
mask = 0X1X010100X001X0011000X10000011X11X1
mem[51007] = 43736089
mem[32553] = 977
mem[5131] = 323347526
mem[21451] = 176282356
mem[22857] = 118
mem[50924] = 217
mask = 001111X110100X11X100000011110111X100
mem[3954] = 3854
mem[19628] = 6778501
mem[29233] = 104
mem[18456] = 135287
mem[10018] = 379
mem[14384] = 969770374
mask = 0X1X000110X1X10001100110100X01000001
mem[25112] = 62086
mem[22964] = 2379583
mem[45021] = 1429003
mask = X010XX11X0110110011XX000011000110001
mem[17265] = 180092
mem[36033] = 495818745
mem[28455] = 7765821
mask = 01000X0110110001100X01X1111000XXX001
mem[395] = 37390
mem[6432] = 962
mem[10247] = 130364
mem[10136] = 529
mem[62469] = 62129
mask = 00001100X010X110011X0000111X0X100XX1
mem[8811] = 206575
mem[37066] = 41
mem[53499] = 1505104
mem[22863] = 59636084
mem[50013] = 45392
mem[29757] = 1343911
mask = 001000011010X10X10000001XX011X000010
mem[23068] = 8829046
mem[49194] = 614470096
mask = XX1X000110X101100X101100X01101X00X11
mem[62870] = 3995829
mem[61328] = 18642
mem[50232] = 70531300
mem[48827] = 17923
mem[12416] = 530017
mem[33496] = 181946
mask = 00100X0110XX00011X00000X101100X10100
mem[64825] = 5590
mem[55315] = 3210
mem[532] = 92226
mask = 00X0XX0000100110011010X0X00XX101011X
mem[35112] = 1037772
mem[46051] = 5636
mem[32440] = 5415168
mem[6812] = 64661
mask = XX1111110010110X010XXX0010X0110XX001
mem[2313] = 14107547
mem[57582] = 3420940
mask = 0X100X01101X011101001001111101110X1X
mem[1584] = 1309601
mem[45021] = 142440
mem[52855] = 22177947
mask = 001X1X11XX101101010010X01X00X100X000
mem[51649] = 224687001
mem[30137] = 16118
mem[49157] = 1286
mask = 0011010X001X0101110X01000X11011X0100
mem[8527] = 483
mem[23222] = 60397
mem[47303] = 4597311
mask = 1010001110X00X0010000X100X0X01011100
mem[61912] = 65321
mem[28793] = 217
mem[3216] = 2226
mem[15267] = 196
mem[12210] = 634690438
mask = XX100001100101011XX000001X0X00X10X10
mem[52112] = 232196
mem[5131] = 8215922
mem[21390] = 97675
mem[60773] = 295919
mem[10967] = 188393052
mem[30137] = 40094772
mask = 01100X00001X0111X11001011X000X1010X0
mem[47036] = 8270917
mem[26111] = 3884
mem[48992] = 3941
mem[21396] = 9612429
mask = X01X01X0100X101001XX00001X1101X00110
mem[47036] = 785762
mem[20586] = 91901152
mem[38530] = 338166139
mem[29577] = 753085
mask = X01001010001011100X01XX010010011X000
mem[50548] = 31352881
mem[17969] = 264
mem[12532] = 122897915
mask = 100X0X011011110001001XX0100110010101
mem[7092] = 488918089
mem[5131] = 2146748
mem[13662] = 1422934
mem[54353] = 299758672
mem[17622] = 15998
mem[12416] = 48024869
mem[15520] = 925305185
mask = X010000100X1000X1000X10010X010100010
mem[49608] = 17989
mem[5478] = 192384
mem[7958] = 729
mask = 0010X001X1100X1X1X000100XX10100X1100
mem[47164] = 170643
mem[1049] = 151435402
mem[24631] = 47998921
mask = 101001XX0X1X010110011101000110110111
mem[39780] = 29719
mem[20606] = 714268
mem[40889] = 367330023
mem[6414] = 28304231
mem[63401] = 1417
mask = 1X10010X00X10101100X010110X1100X1001
mem[38445] = 392
mem[14087] = 19086
mem[36110] = 7609
mem[61683] = 24000
mem[55077] = 2975
mem[2109] = 446867
mask = 011000011X010X0X1X100110100001X1XX10
mem[32849] = 150162
mem[22563] = 3985
mem[10602] = 225990962
mask = 00X00X01X00001X1X0000X00101100100110
mem[43197] = 134523909
mem[65396] = 266246531
mem[54292] = 263069
mem[7677] = 99022189
mem[16568] = 15208393
mask = 1X100X011X1000111000XX1100011X00X0X1
mem[2877] = 1577
mem[39731] = 1276
mem[10602] = 844609393
mem[13447] = 4710
mask = 0010X00110XX0101100X00XX100110X10100
mem[10466] = 93198549
mem[21290] = 624
mem[2948] = 51676784
mem[23734] = 6032
mem[29894] = 48902591
mem[271] = 60066
mask = 1011000110X1011X001X11110100110111X0
mem[1843] = 1562
mem[1049] = 9936
mem[14474] = 305948608
mem[40634] = 680784423
mem[9394] = 12344199
mask = 000011X000100X10011X000X001100X10X10
mem[37198] = 9587
mem[40486] = 15533376
mem[28252] = 1625
mem[59079] = 166206
mask = 0010010110110XXXXX0X0001100100000X00
mem[40119] = 168760390
mem[63012] = 1016
mem[6964] = 13134
mem[6116] = 19700991
mem[60039] = 492285
mask = 10X000011X10001X100000111X10000101X0
mem[532] = 16484832
mem[48228] = 18188385
mem[65048] = 14886349
mem[29631] = 1088356
mask = 0100X00110110X00XXX010110X11X111100X
mem[24637] = 561045
mem[62166] = 62287574
mem[395] = 1350
mem[46447] = 15165
mask = 01101XX100110X1X11001X0X0X0X01011110
mem[29481] = 88712206
mem[8052] = 965421
mask = X0X0010100X1X101101X111010X11010X10X
mem[27388] = 41257883
mem[22151] = 2499234
mem[17067] = 1210879
mask = 00100001X0X10X0110000000101XX001010X
mem[148] = 43621
mem[23734] = 243862817
mask = 101000XX1010001110000X1X00011X0X0100
mem[19226] = 7783454
mem[47036] = 32167689
mem[54708] = 28465363
mem[25775] = 13654
mem[38159] = 226030009
mem[33886] = 22797977
mem[47934] = 34738195
mask = 1010000110110111000101011X0XX1X111X1
mem[8015] = 1639518
mem[32888] = 89628061
mem[19414] = 3293870
mem[45803] = 3055
mem[2849] = 517315
mem[7103] = 1807237
mask = 0010010X1X110111011010001X010X10X110
mem[32337] = 14059
mem[7162] = 22418419
mem[62068] = 491160015
mem[52514] = 62411508
mem[21998] = 16113734
mem[14899] = 4165873
mask = 01000101101001100110XX01011XX0X00XX0
mem[63651] = 706
mem[27388] = 269141496
mem[16791] = 90544
mem[58514] = 2084386
mem[6512] = 82029923
mask = XX100X0X1011011X0110X00000011X10XX10
mem[25492] = 51834825
mem[39104] = 11018
mem[31518] = 5721690
mask = X01001X0101101110X10010111X11X110000
mem[16817] = 43478591
mem[49714] = 32182
mem[7715] = 20391
mem[36282] = 511726
mem[2709] = 58604
mask = 10100001101000XX10001001X110001101X0
mem[21290] = 96121933
mem[4581] = 935753770
mem[10322] = 214308733
mem[22563] = 955
mem[21998] = 174320
mask = 001X010010XXXX10011010010XX01X0X0100
mem[27908] = 10394
mem[58731] = 17043901
mem[12207] = 89277
mem[50189] = 70951683
mem[40310] = 1070062397
mask = X11001X11010X1100110X101X0100X0X01X0
mem[48661] = 35809
mem[6512] = 466
mem[22172] = 9259291
mask = 001X0001X001010XX001000010X1001X1111
mem[45021] = 7965
mem[10414] = 132450
mask = X11001111X10111001X001010X001X00011X
mem[22734] = 23954922
mem[18333] = 522531412
mem[21084] = 2928539
mask = 0010000X00110101XX0XX111100100100110
mem[10793] = 30167743
mem[54236] = 15119211
mem[46526] = 34600696
mask = X010010100X10101X0011X0010000X1XXXX1
mem[40874] = 107825637
mem[12207] = 5066
mem[64061] = 12594443
mem[14677] = 104815480
mem[47294] = 27328513
mem[36871] = 99385
mem[55732] = 3825863
mask = 0010X10X101X011101X000011X1100110000
mem[39282] = 9472566
mem[19564] = 55941
mem[8527] = 26084
mem[10265] = 130187
mem[6432] = 865842
mem[20931] = 1702
mask = 00110X0010X01X1001101X0000XX1XX0110X
mem[54465] = 11299
mem[13022] = 487449
mask = 00100XXX101101101101111010X100100000
mem[20710] = 1510193
mem[1742] = 2963920
mem[15368] = 241191
mem[48928] = 8865
mask = 0X10XX0100X10X011000X01101X0X1101110
mem[33496] = 157963055
mem[10527] = 1744363
mem[25912] = 24812738
mem[53894] = 65229499
mem[27656] = 195539
mem[56053] = 84622
mem[58013] = 503836980
mask = 00X0000X10X1011X0X10110011111X000011
mem[21324] = 100568910
mem[11832] = 25433857
mem[15696] = 65297
mask = 00100X010011010110XX01X11XX1001X0111
mem[1742] = 5701
mem[50038] = 1734
mem[3338] = 10181349
mem[64950] = 715735117
mem[3094] = 6261
mask = 01XX01001010X1X101XX000010X111110XX0
mem[30706] = 34032209
mem[57669] = 953918
mem[2368] = 18511
mem[58246] = 14197924
mem[12602] = 3821248
mem[37932] = 73626
mask = 00X0000110X000011XX0000011100X000000
mem[44726] = 577645454
mem[31822] = 2444199
mask = X0100X01X110001X1X0000X01010X0X00111
mem[40204] = 167462
mem[13234] = 334
mem[55553] = 649450
mem[18698] = 152213289
mem[56964] = 1004699
mem[17434] = 557
mask = 0X1000011111XX01X01XX10X0100000000X1
mem[54363] = 171716
mem[27133] = 813977
mem[25112] = 478238
mem[2734] = 2300
mem[23972] = 7597
mask = 0110X1X1X011011XX10010X11100X1111X00
mem[21998] = 2245
mem[39814] = 10501801
mem[16186] = 807
mask = X010000110100001100X0011X11000XX0010
mem[21976] = 1290104
mem[45127] = 1447
mem[19564] = 679
mem[8927] = 40098844
mem[43124] = 2060353
mem[17227] = 11511
mask = X110X00110X10101111000X0000X10111110
mem[25530] = 23237
mem[55910] = 1785756
mem[38723] = 1821559
mem[30849] = 4089
mem[532] = 661
mask = 0X10010XX01X011X011000011X01X1X001X0
mem[17212] = 6523
mem[37424] = 480
mem[40862] = 449969985
mem[28474] = 40994780
mem[21577] = 36128
mem[39066] = 7501680
mask = X100X101100X01X001101001X11001111110
mem[35142] = 186426
mem[28005] = 1296725
mem[57552] = 433183
mem[26566] = 56636
mem[4581] = 1646680
mem[35799] = 2658
mask = 001011010001X00110X00X10000001011X10
mem[13674] = 62138919
mem[63552] = 11168
mem[11669] = 56357099
mask = 101000XX1010XX0010000X10XX1100010010
mem[22434] = 34054009
mem[19261] = 856
mem[24828] = 7024
mem[34924] = 648168
mem[22917] = 9557844
mask = 10100X0X1011111100X1010000000111100X
mem[63552] = 11477437
mem[23072] = 8131648
mem[19002] = 1064
mem[23946] = 183
mem[2440] = 1277
mask = X0100001101X000X100001001X1X00110110
mem[55553] = 49381
mem[25631] = 41125881
mem[62633] = 590643
mask = 0X1000011001010001XX11X0010100000000
mem[10466] = 506832
mem[23072] = 5583
mem[45005] = 8337603
mem[59216] = 7005456
mask = 0XXX000111111001X0100100001000000100
mem[19928] = 48311172
mem[22974] = 815
mem[34266] = 13786112
mem[1742] = 9648313
mem[1094] = 162
mem[55709] = 31320282
mask = 0XX0000110100XX1X001001001X001000001
mem[22346] = 340600
mem[39104] = 935807
mem[64441] = 570
mem[56853] = 3313
mem[22434] = 1892025
mask = 01X000X1X0110XXXX1100110000X00101000
mem[26813] = 23072664
mem[9142] = 282783543
mem[29807] = 14754
mem[56288] = 62827
mask = 01X0010X1XX001X001X010X011100100X100
mem[47774] = 352023
mem[5938] = 132498542
mem[24828] = 8444211
mem[55829] = 238313735
mask = 0010010100X101X10XX0X10X110100XX1X00
mem[1908] = 30255
mem[40461] = 1524854
mem[21752] = 3313
mem[38177] = 164
mem[32888] = 20182288
mem[17656] = 2560835
mask = 01000101101XX11X0110X10X0000X1XX0000
mem[18028] = 6424701
mem[11832] = 73576
mem[18812] = 15408
mask = X010X00XXXX1X1100100X1101X1111110010
mem[31868] = 1008118155
mem[16970] = 560
mem[6414] = 659729
mask = 00100X0110X1010001X0XXX1X00100100X00
mem[60039] = 1335434
mem[22051] = 4352989
mem[23413] = 8881
mem[5131] = 3574
mem[31132] = 1822377
mem[59227] = 3565275
mem[55044] = 629
mask = 101001010001X101100X1XX01X01100X1111
mem[2119] = 6096
mem[25137] = 4534409
mem[34466] = 2697336
mem[24201] = 506176
mem[25286] = 110343
mask = 0XX00001101100011000010X101X0X11X100
mem[29807] = 25323
mem[12207] = 27513971
mem[9003] = 1398544
mem[28341] = 50817018
mem[30137] = 115
mem[42114] = 67247621
mask = 0000X1000010X1X0011010111X0X01X11X10
mem[48228] = 2010329
mem[45718] = 71839
mem[33886] = 136902
mem[51771] = 2015
mask = X000000110X101000100000010010X0011X0
mem[4243] = 33894587
mem[64857] = 8145
mem[45718] = 97465094
mem[53834] = 3359009
mask = 0010X101000101011X0X01X0101000000001
mem[24012] = 379049
mem[39780] = 26758
mem[59983] = 495835
mem[37409] = 1160
mem[52514] = 6321
mem[27459] = 147
mem[41942] = 217105
mask = 00100001100101011X101X0X10111XX10000
mem[17846] = 10118006
mem[59737] = 2963
mem[34644] = 35114650
mem[13172] = 143244
mem[5938] = 51096
mem[44123] = 3352
mask = 001000X1101101X001X001XX00110101X0X0
mem[38177] = 2168495
mem[11075] = 7671
mem[47735] = 2437651
mem[57709] = 103925776
mem[9577] = 253960744
mem[61912] = 713476954
mem[10466] = 509335816
mask = 0X10X00110010100011000001X01001X0001
mem[46526] = 985771
mem[63247] = 474051554
mem[22968] = 581
mem[29811] = 4967030
mem[57544] = 438283695
mem[7042] = 308851
mask = 001XXX010011X10XX100010X1100X0010100
mem[22856] = 10167
mem[25967] = 196716
mem[17344] = 30111
mem[3954] = 21193
mask = 011000XX1101000X00X00101001111000000
mem[45718] = 22050
mem[4315] = 28856671
mem[3954] = 1669054
mask = XXX000011X11X1000XX011X100X100010101
mem[25208] = 62883413
mem[40039] = 460470
mem[27976] = 317910
mem[6549] = 3697104
mem[34078] = 112
mem[62178] = 479428706
mask = 0110000111X1X0X1X0100100XXX001XX0010
mem[532] = 4184102
mem[25575] = 20376
mem[59465] = 35723765
mem[32827] = 2041066
mem[21963] = 519238
mem[56441] = 22508
mask = X0100001101101000100000X0011X01000X0
mem[34078] = 36026
mem[12451] = 602257
mask = 10X0010100X1X10110010010100X0X100011
mem[52291] = 3349730
mem[51550] = 12311148
mem[27235] = 986707194
mem[7958] = 2162
mem[36824] = 3705422
mask = 011X0X0111010000X110000X1X01XX0XX100
mem[21004] = 1994888
mem[10900] = 11111
mem[24854] = 1327
mem[45320] = 1739644
mem[29894] = 1918
mem[62034] = 165719
mask = 1XX10X10100X1010X1000100X0X10X001X00
mem[54431] = 68179
mem[48498] = 269569
mem[25492] = 53144423
mem[24130] = 510
mem[9579] = 22225
mask = 0X00X101101011X00110X0001001010011X0
mem[54156] = 597982
mem[3020] = 27476
mem[18748] = 105524
mem[37066] = 28361301
mem[43484] = 19990814
mem[18698] = 635178
mask = X0100001101000X1100X0110XX11111X1000
mem[44272] = 88008
mem[11075] = 919
mem[41491] = 2905
mem[4898] = 32296
mem[10607] = 10054
mem[28252] = 31037"""
