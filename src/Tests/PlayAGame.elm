module Tests.PlayAGame exposing (..)

import Array exposing (Array)
import Task


import ClientApi exposing (..)
import Config exposing (..)
import Rest exposing (..)
import SessionModel exposing (..)
import TichuModel exposing (..)


type alias Quad item = (item, item, item, item)


awaitingTablesWithSession s =
    awaitingTables
    |> withHeader "X-Test-Session" s.token


gamesWithSession s =
    games
    |> withHeader "X-Test-Session" s.token


-- 1. create four guest players; remember their tokens
-- 2. create table by first of them; pass 'X-Test-Game-Seed=<constant seed>' header
-- 3. join by the rest
-- 4. all press start
-- 5. play the game (how?)
initPlayAGame seed model =
    let
        tableName = "playAGame" ++ toString seed
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
                |> Task.andThen (\_ ->
                    -- ad. 4
                    Task.map4 (,,,)
                        (startTable s1)
                        (startTable s2)
                        (startTable s3)
                        (startTable s4)
                    |> Task.andThen (\_ ->
                        -- ad. 5
                        Task.map4 (,,,)
                            (getTable s1)
                            (getTable s2)
                            (getTable s3)
                            (getTable s4)
                        |> Task.andThen (\(t1, t2, t3, t4) ->
                            Task.succeed ((s1, s2, s3, s4), (t1, t2, t3, t4))
                        )
                    )
                )
        )
        |> Task.attempt PlayAGameGetSession


type PlayerRequest
    = Pass Session
    | Play Session (List Card)


playRound seed id requests =
    let
        tableName = "playAGame" ++ toString seed
    in
        List.map (\r ->
            case r of
                Pass session ->
                    gamesWithSession session
                    |> postCommand (tableName ++ "/pass")

                Play session list ->
                    gamesWithSession session
                    |> postCommand (tableName ++ "/pass")
        ) requests
        |> Task.sequence
        |> Task.attempt (PlayRound id)


-- UPDATE


type Msg
    -- play a game messages
    = PlayAGameGetSession (RestResult
            ( Quad Session
            , Quad Game
            )
        )
    | PlayRound String (RestResult (List String))


update seed action model =
    case action of
        PlayAGameGetSession result ->
           case Debug.log "PlayAGameStart!" result of
               Ok ((s1, s2, s3, s4), (t1, t2, t3, t4)) ->
                   { model | sessions = Array.fromList [ s1, s2, s3, s4 ] }
                   ! [ firstRound seed s1 s2 s3 s4 ]
               Err _ ->
                   model ! []

        PlayRound id result ->
            let
                x = Debug.log "game" result
            in
                model ! []


firstRound seed s1 s2 s3 s4 =
    playRound seed "round1"
        [ Play s1 [ MahJong ]
        ]


