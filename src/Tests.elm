module Tests exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Navigation
import Json.Decode as Json exposing (decodeString)
import Result exposing (toMaybe)
import Random
import Task exposing (..)
import Time exposing (Time, every, second, now)


import ClientApi exposing (..)
import Config exposing (..)
import Component exposing (..)
import BaseModel exposing (..)
import Rest exposing (..)
import SessionModel exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import TableView exposing (..)
import Tests.PlayAGame as PlayAGame


component : Component Model msg Msg
component =
    Component model update view (Just init) Nothing



-- MODEL


type alias Model =
    { seed : Int
    , awaitingTable : Maybe AwaitingTable
    , game : Maybe Game
    , lastFinishedTableUpdate : Maybe Time
    , deserializedGame : Result String Game
    , deserializedAwaitingTable : Result String AwaitingTable
    , playAGame : GameState
    }


type alias GameState =
    { sessions : Array Session
    , playerState : List Game
    , game : Maybe (Result String Game)
    , internal : Game
    }


model : Model
model =
    Model 0
        Nothing
        Nothing
        Nothing
        (initGame "TestGame" 0 []
            |> encodeGame
            |> decodeString gameDecoder
        )
        (AwaitingTable "AwaitingTable" [ AwaitingTableUser "player one" 0 False ] True 0
            |> encodeAwaitingTable
            |> decodeString awaitingTableDecoder
        )
        (GameState Array.empty [] Nothing (initGame "test" 0 []))


init : Context msg Msg -> Cmd msg
init ctx =
  Random.generate BaseRandom (Random.int 0 Random.maxInt)
    |> Cmd.map ctx.mapMsg


type alias Quad item = (item, item, item, item)


-- UPDATE


type Msg
    = BaseRandom Int
    | UpdateTables (RestResult ( Time, Result Game AwaitingTable ))
    | CheckUpdate Time
    -- play a game messages
    | PlayAGame PlayAGame.Msg


mapPlayAGame ctx =
    PlayAGame >> ctx.mapMsg


update : ComponentUpdate Model msg Msg
update ctx action model =
    case action of
        PlayAGame m ->
            let
                (playAGame, cmd) =
                    PlayAGame.update
                        (Context <| mapPlayAGame ctx)
                        m
                        model.playAGame
            in
                ({ model | playAGame = playAGame }, cmd)

        BaseRandom int ->
            let
                newModel = { model | seed = int }
            in
                newModel ! [ PlayAGame.initPlayAGame newModel |> Cmd.map (mapPlayAGame ctx) ]

        UpdateTables result ->
            case result of
                Ok ( time, res ) ->
                    let
                        newModel =
                            { model
                                | game = Nothing
                                , awaitingTable = Nothing
                                , lastFinishedTableUpdate = Just time
                            }
                    in
                        case res of
                            Ok awaitingTable ->
                                { newModel | awaitingTable = Just awaitingTable } ! []

                            Err game ->
                                { newModel | game = Just game } ! []

                _ ->
                    model ! []

        CheckUpdate time ->
            case model.lastFinishedTableUpdate of
                Just last ->
                    if last + (3 * second) < time then
                        { model
                            | lastFinishedTableUpdate = Nothing
                        }
                            ! [ init ctx ]
                    else
                        model ! []

                _ ->
                    model ! []



-- VIEW


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


maybeResultSuccess result =
    Maybe.map resultSuccess result


testHeader name passed =
    maybeTestHeader name (Just passed)


resultSuccess result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


displayResult element =
    div []
        [ case element of
            Just game ->
                text <| toString element

            Nothing ->
                i [ (class "fa fa-spinner fa-spin fa-fw") ] []
        ]


view : ComponentView Model msg Msg
view ctx model =
    Page ("Tests [seed: " ++ (toString model.seed) ++ "]") <|
        fullRow
            [ testHeader "serialize/deserialize Game" (resultSuccess model.deserializedGame)
            , div [] [ text <| toString model.deserializedGame ]
            , testHeader "serialize/deserialize AwaitingTable" (resultSuccess model.deserializedAwaitingTable)
            , div [] [ text <| toString model.deserializedAwaitingTable ]
            , maybeTestHeader "Play a game" (maybeResultSuccess model.playAGame.game)
            , displayResult model.playAGame.game
            ]


