module TestBasics exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


import SessionModel exposing (..)
import TichuModel exposing (..)


type alias GameState =
    { sessions : Maybe (Quad Session)
    , playerState : List Game
    , result : Maybe (Result String String)
    }


type alias Quad item = (item, item, item, item)


quadGet i (q1, q2, q3, q4) =
    case i of
        1 -> q1
        2 -> q2
        3 -> q3
        4 -> q4
        _ -> Debug.crash "Wrong item"


quadMap : (a -> b) -> Quad a -> Quad b
quadMap mapper (q1, q2, q3, q4) =
    (mapper q1, mapper q2, mapper q3, mapper q4)


toList (q1, q2, q3, q4) =
    [ q1, q2, q3, q4 ]


quadZip : Quad a -> Quad b -> Quad (a, b)
quadZip (q1, q2, q3, q4) (s1, s2, s3, s4) =
    ((q1, s1), (q2, s2), (q3, s3), (q4, s4))


resultSuccess result =
    case result of
        Ok _ -> True
        Err _ -> False


maybeResultSuccess result =
    Maybe.map resultSuccess result


maybeTestHeader name passed =
    let
        color =
            case passed of
                Just p ->
                    if p then "green" else "red"
                Nothing ->
                    "grey"
    in
        h4 []
            [ span [ style [("color", color)] ] [
                text
                (case passed of
                    Just p ->
                        (if p then
                            "[SUCC] "
                          else
                            "[FAIL] "
                         )

                    Nothing ->
                        "[....] "
                )
                ]
            , text name
            ]


testHeader name passed =
    maybeTestHeader name (Just passed)


displayResult element =
    div []
        [ case element of
            Just data ->
                text <| toString data

            Nothing ->
                i [ (class "fa fa-spinner fa-spin fa-fw") ] []
        ]


