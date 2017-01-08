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


-- 1. create four guest players; remember their tokens
-- 2. create table by first of them; pass 'X-Test-Game-Seed=<constant seed>' header
-- 3. join by the rest
-- 4. all press start
-- 5. play the game (how?)
initPlayAGame model =
    let
        awaitingTablesWithSession s =
            awaitingTables
            |> withHeader "X-Test-Session" s.token
        gamesWithSession s =
            games
            |> withHeader "X-Test-Session" s.token
        tableName = "playAGame" ++ toString model.seed
        getGuestToken =
            sessions
            |> withQueryParams
                [ ("noHeader", "true")
                , ("forceNew", "true")
                , ("seed", "0")
                ]
            |> get "guest"
        joinTable session =
            awaitingTablesWithSession session
            |> postCommand (tableName ++ "/join")
        startTable session =
            awaitingTablesWithSession session
            |> postCommand (tableName ++ "/start")
        getTable session =
            gamesWithSession session
            |> get tableName
    in
        -- ad. 1
        Task.map4 (,,,)
            getGuestToken
            getGuestToken
            getGuestToken
            getGuestToken
        |> Task.andThen (\(s1, s2, s3, s4) ->
            Task.map5 (,,,,)
                -- ad. 2
                (awaitingTablesWithSession s1
                 |> withHeader "X-Test-Game-Seed" "0"
                 |> postCommand tableName)
                -- ad. 3
                (joinTable s1)
                (joinTable s2)
                (joinTable s3)
                (joinTable s4)
                |> Task.andThen (\(cmd, t1, t2, t3, t4) ->
                    -- ad. 4
                    Task.map4 (,,,)
                        (startTable s1)
                        (startTable s2)
                        (startTable s3)
                        (startTable s4)
                    |> Task.andThen (\(st1, st2, st3, st4) ->
                        -- ad. 4
                        Task.map4 (,,,)
                            (getTable s1)
                            (getTable s2)
                            (getTable s3)
                            (getTable s4)
                        |> Task.andThen (\(g1, g2, g3, g4) ->
                            Task.succeed ((s1, s2, s3, s4), (t1, t2, t3, t4))
                        )
                    )
                )
        )
        |> Task.attempt PlayAGameGetSession


-- UPDATE


type Msg
    = BaseRandom Int
    | UpdateTables (RestResult ( Time, Result Game AwaitingTable ))
    | CheckUpdate Time
    -- play a game messages
    | PlayAGameGetSession (RestResult
            ( Quad Session
            , Quad String
            )
        )
    | PlayAGameOpenTable (RestResult Game)


update : ComponentUpdate Model msg Msg
update ctx action model =
    case action of
        PlayAGameGetSession result ->
            let
                playAGame = model.playAGame
            in
                case result of
                    Ok ((s1, s2, s3, s4), (t1, t2, t3, t4)) ->
                        { model
                        | playAGame = { playAGame | sessions = Array.fromList [ s1, s2, s3, s4 ] }
                        } ! [ games
                              |> withHeader "X-Test-Session" s1.token
                              |> get ("playAGame" ++ toString model.seed)
                              |> Task.attempt (\r -> ctx.mapMsg <| PlayAGameOpenTable r) ]
                    Err _ ->
                        { model | seed = model.seed } ! []


        PlayAGameOpenTable table ->
            let
                x = Debug.log "game" table
            in
                model ! []

        BaseRandom int ->
            let
                newModel = { model | seed = int }
            in
                newModel ! [ initPlayAGame newModel |> Cmd.map ctx.mapMsg ]

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
