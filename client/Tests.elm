module Tests exposing (Model, Msg(..), component, emptyModel, init, mapPlayAGame, update, view)

import BaseModel exposing (..)
import Browser.Navigation
import ClientApi exposing (..)
import Component exposing (..)
import Config exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Json
import Random
import Rest exposing (..)
import Result exposing (toMaybe)
import SessionModel exposing (..)
import TableView exposing (..)
import Task exposing (..)
import TestBasics exposing (..)
import Tests.Combinations as CardCombinations
import Tests.PlayAGame as PlayAGame
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import Time exposing (Posix, every, now)


component : Component Model msg Msg
component =
    Component emptyModel update view (Just init) Nothing



-- MODEL


type alias Model =
    { seed : Int
    , awaitingTable : Maybe AwaitingTable
    , game : Maybe Game
    , lastFinishedTableUpdate : Maybe Posix
    , deserializedGame : Result String Game
    , deserializedAwaitingTable : Result String AwaitingTable
    , playAGame : GameState
    }


stringifyError result =
    case result of
        Ok v ->
            Ok v

        Err v ->
            Err (Json.errorToString v)


emptyModel : Model
emptyModel =
    Model 0
        Nothing
        Nothing
        Nothing
        (initGame "TestGame" (GameConfig Humans) True 0 []
            |> encodeGame
            |> decodeString gameDecoder
            |> stringifyError
        )
        (AwaitingTable
            "AwaitingTable"
            (GameConfig Humans)
            [ AwaitingTableUser "player one" (Time.millisToPosix 0) False True ]
            True
            0
            |> encodeAwaitingTable
            |> decodeString awaitingTableDecoder
            |> stringifyError
        )
        (GameState Nothing [] Nothing)


init : Context msg Msg -> Cmd msg
init ctx =
    Random.generate BaseRandom (Random.int 0 Random.maxInt)
        |> Cmd.map ctx.mapMsg



-- UPDATE


type Msg
    = BaseRandom Int
    | UpdateTables (RestResult ( Posix, Result Game AwaitingTable ))
    | CheckUpdate Posix
      -- play a game messages
    | PlayAGame PlayAGame.Msg


mapPlayAGame ctx =
    PlayAGame >> ctx.mapMsg


update : ComponentUpdate Model msg Msg
update ctx action model =
    case action of
        PlayAGame m ->
            let
                ( playAGame, cmd ) =
                    PlayAGame.update
                        model.seed
                        m
                        model.playAGame
            in
            ( { model | playAGame = playAGame }
            , Cmd.map (mapPlayAGame ctx) cmd
            )

        BaseRandom int ->
            let
                newModel =
                    { model | seed = int }
            in
            ( newModel
            , PlayAGame.initPlayAGame int newModel |> Cmd.map (mapPlayAGame ctx)
            )

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
                            ( { newModel | awaitingTable = Just awaitingTable }
                            , Cmd.none
                            )

                        Err game ->
                            ( { newModel | game = Just game }
                            , Cmd.none
                            )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CheckUpdate time ->
            case model.lastFinishedTableUpdate of
                Just last ->
                    if Time.posixToMillis last + (3 * second) < Time.posixToMillis time then
                        ( { model
                            | lastFinishedTableUpdate = Nothing
                          }
                        , init ctx
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                _ ->
                    ( model
                    , Cmd.none
                    )


second =
    1000



-- VIEW


view : ComponentView Model msg Msg
view ctx model =
    Page ("Tests [seed: " ++ Debug.toString model.seed ++ "]") <|
        fullRow
            ([ testHeader "serialize/deserialize Game" (resultSuccess model.deserializedGame)
             , div [] [ text <| Debug.toString model.deserializedGame ]
             , testHeader "serialize/deserialize AwaitingTable" (resultSuccess model.deserializedAwaitingTable)
             , div [] [ text <| Debug.toString model.deserializedAwaitingTable ]
             ]
                ++ CardCombinations.testCombinations
                ++ PlayAGame.view (Context <| mapPlayAGame ctx) model.playAGame
            )
