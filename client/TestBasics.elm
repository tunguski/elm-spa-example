module TestBasics exposing (GameState, Quad, displayResult, maybeResultSuccess, maybeTestHeader, quadMap, quadZip, resultSuccess, testHeader, toList)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import SessionModel exposing (..)
import TichuModel exposing (..)


type alias GameState =
    { sessions : Maybe (Quad Session)
    , playerState : List Game
    , result : Maybe (Result String String)
    }


type alias Quad item =
    { e1 : item
    , e2 : item
    , e3 : item
    , e4 : item
    }


quadMap : (a -> b) -> Quad a -> Quad b
quadMap mapper quad =
    Quad
        (mapper quad.e1)
        (mapper quad.e2)
        (mapper quad.e3)
        (mapper quad.e4)


toList quad =
    [ quad.e1, quad.e2, quad.e3, quad.e4 ]


quadZip : Quad a -> Quad b -> Quad ( a, b )
quadZip q1 q2 =
    Quad
        ( q1.e1, q2.e1 )
        ( q1.e2, q2.e2 )
        ( q1.e3, q2.e3 )
        ( q1.e4, q2.e4 )


resultSuccess result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


maybeResultSuccess result =
    Maybe.map resultSuccess result


maybeTestHeader name passed =
    let
        color =
            case passed of
                Just p ->
                    if p then
                        "green"

                    else
                        "red"

                Nothing ->
                    "grey"
    in
    h4 []
        [ span [ style "color" color ]
            [ text
                (case passed of
                    Just p ->
                        if p then
                            "[SUCC] "

                        else
                            "[FAIL] "

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
                text <| Debug.toString data

            Nothing ->
                i [ class "fa fa-spinner fa-spin fa-fw" ] []
        ]
