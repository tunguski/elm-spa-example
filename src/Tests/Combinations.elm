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
                trick |> Maybe.map (\s -> True)
        in
            div []
                [ maybeTestHeader ("Parse trick: " ++ name) passed
                ]
        )
        [ testFullHouse
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


checkCombination name cards =
    (name, parseTrick cards)


