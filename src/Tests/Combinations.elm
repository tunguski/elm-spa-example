module Tests.Combinations exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


import TichuModel exposing (..)
import TestBasics exposing (..)


testCombinations =
    List.map (\(name, trick) ->
        let
            passed =
                case trick of
                    Just _ -> Just True
                    _ -> Just False
        in
            div []
                [ maybeTestHeader ("Parse trick: " ++ name) passed
                ]
        )
        [ testFullHouse
        , testFullHouseWithPhoenix
        , testPairStairs
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


checkCombination name cards =
    (name, parseTrick cards)


