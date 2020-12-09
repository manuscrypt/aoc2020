module Main exposing (main)

import Array
import Html exposing (Html, div, text)
import List
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, keyword, oneOf, succeed)


main : Html.Html msg
main =
    let
        ( nums, pre ) =
            parseInput input

        partA =
            findMatchIn ( nums, pre ) 0
                |> Maybe.withDefault -1

        partB =
            findWeakness partA nums
    in
    div []
        [ output "part1"
        , output (String.fromInt partA)
        , output "part2"
        , output (String.fromInt partB)
        ]


findWeakness : Int -> List Int -> Int
findWeakness target nums =
    let
        resultList =
            solveB nums target |> Maybe.withDefault [] |> List.sort

        ( head, last ) =
            ( List.head resultList |> Maybe.withDefault -1
            , List.last resultList |> Maybe.withDefault -1
            )
    in
    head + last


solveB all target =
    all
        |> List.indexedMap (\i f -> buildSum i 2 target all)
        |> List.filterMap identity
        |> List.head


buildSum : Int -> Int -> Int -> List Int -> Maybe (List Int)
buildSum from count target list =
    let
        arr =
            subList from count list

        sum =
            List.sum arr
    in
    if sum < target then
        buildSum from (count + 1) target list

    else if sum == target then
        Just arr

    else
        Nothing


subList from count list =
    Array.fromList list
        |> Array.slice from (from + count)
        |> Array.toList


findMatchIn : ( List Int, Int ) -> Int -> Maybe Int
findMatchIn ( numbers, preamble ) idx =
    if idx < preamble then
        findMatchIn ( numbers, preamble ) (idx + 1)

    else
        case List.getAt idx numbers of
            Nothing ->
                Nothing

            Just target ->
                case
                    findMatch
                        (numbers
                            |> Array.fromList
                            |> Array.slice (idx - preamble) idx
                            |> Array.toList
                        )
                        target
                of
                    Nothing ->
                        Just target

                    Just pair ->
                        findMatchIn ( numbers, preamble ) (idx + 1)


findMatch : List Int -> Int -> Maybe ( Int, Int )
findMatch numbers target =
    List.uniquePairs numbers
        |> List.filter (\( a, b ) -> a + b == target)
        |> List.head


output : String -> Html msg
output s =
    div [] [ text s ]



--input


parseInput : ( String, Int ) -> ( List Int, Int )
parseInput ( str, pre ) =
    ( str
        |> String.lines
        |> List.filterMap String.toInt
    , pre
    )


sampleA : ( String, Int )
sampleA =
    ( """35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576""", 5 )


input : ( String, Int )
input =
    ( """10
38
48
24
43
25
11
35
29
28
41
30
1
14
40
13
47
2
23
9
27
16
6
12
33
3
4
5
38
7
44
36
43
8
10
31
50
15
11
52
74
13
14
17
18
9
19
16
20
21
12
28
22
23
24
35
46
25
26
37
27
39
64
43
30
41
29
48
33
42
31
38
47
32
34
57
84
45
49
50
51
104
70
56
89
80
60
59
61
78
62
63
88
72
93
65
66
99
77
147
106
94
112
100
141
107
117
171
116
120
119
126
121
124
181
128
129
131
138
213
142
143
176
257
225
281
194
238
258
223
224
233
235
236
239
240
245
255
253
305
260
421
314
378
285
318
366
370
655
417
418
429
447
565
459
457
524
485
475
609
777
498
540
793
681
545
599
603
1236
876
684
736
787
1264
835
847
1202
906
957
1374
1693
1332
960
2568
1038
1148
1043
1085
1434
1450
1283
1287
1890
1420
1471
2167
1523
1622
1804
2928
1753
2661
2186
1995
2752
2326
2905
3557
2081
2707
3091
2128
2368
2703
3042
2909
2758
2891
3093
4925
3145
6462
3990
9214
3748
4987
4076
4123
5419
8590
5638
5998
4209
4449
4496
5273
5221
5951
5461
7387
7207
7542
8113
7641
9096
6893
7738
13663
7824
9396
8199
11016
8332
10447
13592
10911
8658
11850
12562
13003
15874
14100
12668
12354
14534
14435
14631
17220
15092
18779
23093
15562
16023
24488
21000
16531
16990
19105
26888
21012
37023
20508
24204
26662
36842
25022
40313
26789
27446
28969
29066
31623
30654
32082
39766
31585
47789
47396
33521
36095
71898
37498
39613
50078
65811
44712
45530
50866
51684
55758
87343
86961
66402
56415
81663
59720
65106
62239
63667
67680
137709
109818
69616
71019
73593
77111
82210
95371
125277
90242
118305
121521
121959
107442
127345
118654
116135
120082
123387
234787
190383
174924
198364
192028
209555
151826
140635
272593
144612
150704
316669
302530
250732
208547
273347
260747
226096
252054
292461
369386
326750
236217
296438
396734
285247
295316
291339
332663
336640
429859
597387
467385
353159
487344
386921
552086
524079
648475
618089
722320
462313
488271
631956
583800
622066
521464
527556
576586
820934
580563
644498
1046113
762522
689799
815472
740080
820544
1459970
874265
849234
1274406
1139553
1143530
950584
983777
1964205
1438855
1049020
1098050
1149622
1108119
1221084
1957353
1407020
2192550
1334297
1452321
1429879
1505271
2313120
1560624
2372456
1898254
2100206
1799818
1934361
1999604
2032797
2048634
2081827
3478513
2157139
2370706
4565006
2257741
2329203
2859341
3734179
2741317
2935150
2786618
6665212
4345917
3065895
6544408
4339568
6564610
3698072
3799422
3832615
4328807
7405463
5779899
4414880
4238966
5264353
5092289
4586944
4999058
5044359
5070520
9851297
5527935
8903135
5721768
5852513
8419559
8685485
10904178
7497494
8026879
7530687
7632037
12669816
8071581
9001824
14537998
8653846
8825910
9238024
9586002
15379232
9631303
10766127
17339331
10923033
13484550
19342592
16323404
13252455
13350007
32692599
15028181
18239848
15602268
16098460
15162724
19217305
22609579
16725427
22981310
17479756
17891870
18063934
21689160
20352129
31126641
23115853
24018582
39841280
24175488
26602462
28280636
28512731
28415179
28378188
31700728
30190905
30764992
31261184
40273948
31888151
34205183
34617297
51478770
54682038
35371626
35955804
55723755
52396770
55719310
47134435
52299218
60807645
50777950
52456124
59773915
63149335
67259777
58569093
59143180
60955897
61452089
105459988
82739954
66093334
66505448
68822480
69988923
71327430
82506061
102858190
83090239
97912385
99433653
99590559
103234074
103077168
122105380
120021182
157055565
118917095
117712273
88311122
201423156
165683893
127545423
151912719
182096620
149011509
320340251
135327928
140149910
154417669
153833491
170817183
218350748
202667727
187744775
187901681
440361433
191388290
336063021
206023395
207228217
215856545
223639050
293229316
242728791
262873351
267695333
328051591
275477838
594194924
647589650
294567579
351184473
293983401
308251160
324650674
358561958
801423141
375646456
397411685
652545359
557207868
559702071
413251612
421879940
423084762
524107705
802430862
505602142
632901834
1454976221
543173171
588550980
737902286
979087808
700297130
659435633
602234561
1145758848
666813118
683212632
771813570
954619553
1161936632
810663297
945987645
844964702
972953683
835131552
927482082
1326538567
1029709847
1048775313
1094153122
1131724151
1516033062
1145407732
1447199263
1326248751
1629200277
1261670194
1269047679
2541352385
1518344184
1350025750
1455026202
1582476867
2067106805
1655627999
1645794849
1883906865
3951013670
2842571629
1762613634
2676274501
2078485160
2317822992
2180499464
2663751916
2393394345
2917298193
3110654201
2619073429
3238104866
2530717873
2780014378
2724073881
2805051952
2868369934
2932502617
4588130616
3228271716
4080436626
3943113098
3408408483
3646520499
5335769825
7954843254
4293331507
5890668579
4258984624
4498322456
4985551416
4924112218
5012467774
6804510507
5310732251
5149791302
5254791754
5399087807
5504088259
8928664514
12140280332
6874792215
6636680199
9797419766
7054928982
8023549724
7351521581
7667393107
7905505123
14183456268
9278882923
8552316131
8757307080
9271452398
15690942831
9909663634
9936579992
10267259528
12140768458
17842085115
16573260191
10758880013
14304073306
13929721197
13511472414
13691609181
30502981388
16434099965
14722322089
14406450563
15018914688
15257026704
24631985723
19208032390
32899641571
23685333486
24688601210
18028759478
28787639491
26444841764
26547219021
28714028649
42878941729
22899648471
27332140204
30738173271
24270352427
28413931270
33285786182
52684283697
44848031235
29741236777
42717360688
29663477267
39889012427
30275941392
50817571448
37236791868
40928407949
46742788127
54961150291
56288455798
42299111905
47170000898
58455265426
49446867492
50231788675
96189655619
66900269135
51602492631
53933829694
54011589204
58077408537
87059013325
59404714044
59939418659
60017178169
109464045661
102316290074
67512733260
77018729519
96616868390
152548078749
83227519854
89041900032
163397875355
89469112803
113951007863
147924378229
99678656167
119421892213
104165618369
105536322325
135096138056
111007206675
118094586706
119956596828
119344132703
126917447304
156981846063
156554633292
137035907688
196295524557
167191389427
144531462779
301831846882
271969970962
172269419886
172696632657
178511012835
189147768970
193634731172
213629664030
203844274536
223630909031
246103344731
209701940694
237924653979
229101793381
230351339378
237438719409
239300729531
246261580007
417473938566
281567370467
316800882665
405116043406
317228095436
311722852206
323042475614
344966052543
350780432721
351207645492
426586488379
372145744007
403336671866
397479005708
413546215230
441768928515
461555563010
591227632550
438803734075
642554762815
459453132759
467790058787
476739448940
604609846081
527828950474
593290222673
598795465903
959355645480
628950947642
634765327820
723353389499
668008528157
839247934223
701988078213
918508377455
769624749715
1295278300886
880572662590
811025220938
852349949305
900359297085
966632684549
906593792862
1110344821602
927243191546
936192581699
944529507727
1479033749095
1195837478631
1121119173147
1263716275462
1296959475799
1302773855977
1358118717319
1534378610437
1369996606370
1437633277872
1554338027518
1729533598393
1621974699020
2837152466414
1663375170243
1691597883528
1738268412484
2208245783189
2021478470232
1833836984408
2818054302980
4481429473223
2230017047523
1880722089426
2065648680874
2316956651778
3026493074192
2384835448609
3059607976892
2740407133849
2660892573296
2728115323689
2807629884242
3544097259669
3129231161400
3176312726538
3401643582727
3285349869263
3525434867936
4682371043528
4042082767597
3803917093358
3714559073834
4150793636186
3899485665282
4701792100387
3946370770300
5446187813178
4608837413115
4977849225074
5057363785627
5401299707145
5045728021905
5389007896985
5983942610780
7878104826925
6674486093989
8263199094337
6927078450663
6414581030663
8084014626255
6810784737199
7231720639563
7239993941770
11392430255737
8781766318432
7518476167192
7614044739116
16438158277642
9335378667285
8555208183415
8992098792205
12217843166844
9654565435020
10023577246979
10103091807532
10434735918890
13215663250343
13223936552550
14292685857588
13089067124652
13225365767862
13341659481326
14167072392433
15406679822868
14042505376762
14050778678969
14750196806755
15132520906308
16169252922531
16073684350607
18646664227225
16606143531321
19757657242552
17547306975620
19095190599737
19015676039184
33262262992170
20089301353910
23192158932184
23327028360082
28495746947520
49431515914701
32239612591734
26314432892514
26430726605978
27384164858088
34501870422605
30336325314964
28800975485724
28792702183517
29183299585277
29882717713063
31206205256915
32242937273138
39798302463505
38735965581135
48882003537427
39104977393094
36562983014804
68447191953885
42207834971368
43281460286094
50425626668874
55431771523918
49641461252596
52745159498492
61088922969978
53698597750602
53814891464066
55223428789495
57720490173052
57593677669241
67897679576611""", 25 )
