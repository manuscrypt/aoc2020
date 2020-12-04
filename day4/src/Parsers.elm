module Parsers exposing (EyeColor(..), Height(..), Passport, parseInput, splitLines)

import List.Extra as List
import Parser exposing ((|.), (|=), Parser, chompWhile, getChompedString, int, keyword, map, oneOf, spaces, succeed, symbol)


type Height
    = Cm Int
    | Inch Int


type EyeColor
    = Amb
    | Blu
    | Brn
    | Gry
    | Grn
    | Hzl
    | Oth


type alias Passport =
    { byr : Int
    , cid : Maybe String
    , ecl : EyeColor
    , eyr : Int
    , hcl : String
    , hgt : Height
    , iyr : Int
    , pid : String
    }


passport : Parser Passport
passport =
    succeed Passport
        |= parseInt "byr"
        |. spaces
        |= parseMaybeString "cid"
        |. spaces
        |= parseEyeColor "ecl"
        |. spaces
        |= parseInt "eyr"
        |. spaces
        |= parseHairColor "hcl"
        |. spaces
        |= parseHeight "hgt"
        |. spaces
        |= parseInt "iyr"
        |. spaces
        |= parseString "pid"


parseEyeColor : String -> Parser EyeColor
parseEyeColor str =
    succeed identity
        |. keyword str
        |. symbol ":"
        |= oneOf
            [ succeed Amb |. keyword "amb"
            , succeed Blu |. keyword "blu"
            , succeed Brn |. keyword "brn"
            , succeed Gry |. keyword "gry"
            , succeed Grn |. keyword "grn"
            , succeed Hzl |. keyword "hzl"
            , succeed Oth |. keyword "oth"
            ]


parseHairColor : String -> Parser String
parseHairColor str =
    succeed identity
        |. keyword str
        |. symbol ":"
        |. symbol "#"
        |= (chompWhile isValidDigitOrLetter |> getChompedString)


isValidDigitOrLetter : Char -> Bool
isValidDigitOrLetter c =
    List.member c [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' ]


parseHeight : String -> Parser Height
parseHeight str =
    parseString str
        |> Parser.andThen checkHeight


checkHeight : String -> Parser Height
checkHeight s =
    case String.right 2 s of
        "in" ->
            succeed
                (String.toInt (String.dropRight 2 s)
                    |> Maybe.withDefault 0
                    |> Inch
                )

        "cm" ->
            succeed
                (String.toInt (String.dropRight 2 s)
                    |> Maybe.withDefault 0
                    |> Cm
                )

        _ ->
            Parser.problem "not a valid height"


parseInt : String -> Parser Int
parseInt key =
    succeed identity
        |. keyword key
        |. symbol ":"
        |= int


parseString : String -> Parser String
parseString key =
    succeed identity
        |. keyword key
        |. symbol ":"
        |= (chompWhile (\c -> c /= ' ' && c /= '\u{000D}' && c /= '\n') |> getChompedString)


parseMaybeString : String -> Parser (Maybe String)
parseMaybeString key =
    Parser.oneOf
        [ Parser.map Just (parseString key)
        , Parser.map (\_ -> Nothing) spaces
        ]


parseInput : String -> List Passport
parseInput str =
    splitLines str
        |> List.filterMap parseLine


parseLine : String -> Maybe Passport
parseLine line =
    String.words line
        |> List.sort
        |> String.join " "
        |> Parser.run passport
        |> Result.toMaybe


splitLines : String -> List String
splitLines inp =
    let
        ( c, a ) =
            inp
                |> String.lines
                |> List.foldl
                    (\line ( cur, all ) ->
                        if line == "" then
                            ( "", cur :: all )

                        else
                            ( line ++ " " ++ cur, all )
                    )
                    ( "", [] )
    in
    List.reverse (c :: a)
