module Tests.PlayAGame exposing (..)

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
                        -- ad. 5
                        Task.map4 (,,,)
                            (getTable s1)
                            (getTable s2)
                            (getTable s3)
                            (getTable s4)
                        |> Task.andThen (\_ ->
                            -- ad. 5
                            Task.map4 (,,,)
                                (getTable s1)
                                (getTable s2)
                                (getTable s3)
                                (getTable s4)
                            |> Task.andThen (\_ ->
                                Task.succeed ((s1, s2, s3, s4), (t1, t2, t3, t4))
                            )
                        )
                    )
                )
        )
        |> Task.attempt PlayAGameGetSession


-- UPDATE


type Msg
    -- play a game messages
    = PlayAGameGetSession (RestResult
            ( Quad Session
            , Quad String
            )
        )
    | PlayAGameOpenTable (RestResult Game)


update ctx action model =
    case action of
        PlayAGameGetSession result ->
           case Debug.log "PlayAGameStart!" result of
               Ok ((s1, s2, s3, s4), (t1, t2, t3, t4)) ->
                   { model | sessions = Array.fromList [ s1, s2, s3, s4 ] } ! []
               Err _ ->
                   model ! []


        PlayAGameOpenTable table ->
            let
                x = Debug.log "game" table
            in
                model ! []


