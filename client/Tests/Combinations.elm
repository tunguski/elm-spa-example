module Tests.Combinations exposing (checkCombination, testCombinations, testFullHouse, testFullHouseWithPhoenix, testPairStairs, testStraightWithMahJong)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import TestBasics exposing (..)
import TichuModel exposing (..)


testCombinations =
    List.map
        (\( name, trick ) ->
            let
                passed =
                    case trick of
                        Just _ ->
                            Just True

                        _ ->
                            Just False
            in
            div []
                [ maybeTestHeader ("Parse trick: " ++ name) passed
                ]
        )
        [ testFullHouse
        , testFullHouseWithPhoenix
        , testPairStairs
        , testStraightWithMahJong
        ]


testFullHouse =
    checkCombination
        "Full House"
        [ NormalCard Diamonds (R 4)
        , NormalCard Spades (R 4)
        , NormalCard Hearts (R 5)
        , NormalCard Spades (R 5)
        , NormalCard Clubs (R 5)
        ]


testFullHouseWithPhoenix =
    checkCombination
        "Full House with Phoenix"
        [ NormalCard Clubs J
        , NormalCard Diamonds J
        , NormalCard Diamonds K
        , NormalCard Spades K
        , Phoenix
        ]


testPairStairs =
    checkCombination
        "Pair stairs"
        [ NormalCard Clubs (R 6)
        , NormalCard Diamonds (R 6)
        , NormalCard Hearts (R 7)
        , NormalCard Spades (R 7)
        ]


testStraightWithMahJong =
    checkCombination
        "Straight with MahJong"
        [ MahJong
        , NormalCard Hearts (R 2)
        , NormalCard Diamonds (R 3)
        , NormalCard Clubs (R 4)
        , NormalCard Spades (R 5)
        , NormalCard Spades (R 6)
        , NormalCard Spades (R 7)
        , NormalCard Hearts (R 8)
        ]


checkCombination name cards =
    ( name, parseTrick cards )
