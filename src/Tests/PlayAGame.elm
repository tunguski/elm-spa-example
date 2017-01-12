module Tests.PlayAGame exposing (..)

import Array exposing (Array)
import Http exposing (Error)
import Task exposing (Task)


import ClientApi exposing (..)
import Config exposing (..)
import Rest exposing (..)
import SessionModel exposing (..)
import TichuModel exposing (..)


type alias Quad item = (item, item, item, item)


getTableName seed =
    "playAGame" ++ toString seed


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
        tableName = getTableName seed
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
            -- ad. 2
            (awaitingTablesWithSession s1
             |> withHeader "X-Test-Game-Seed" "0"
             |> postCommand tableName)
            |> andThenReturn
                -- ad. 3
                (execForAll joinTable s1 s2 s3 s4)
            |> andThenReturn
                -- ad. 4
                (execForAll startTable s1 s2 s3 s4)
            |> andThenReturn
                -- ad. 5
                (execForAll getTable s1 s2 s3 s4)
            |> Task.andThen (\games ->
                Task.succeed ((s1, s2, s3, s4), games)
            )
        )
        |> Task.attempt PlayAGameGetSession


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


type PlayerRequest
    = Pass Session
    | Play Session (List Card)


playRound : Int -> List PlayerRequest -> Task Error (List String)
playRound seed requests =
    let
        tableName = getTableName seed
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


execForAll function s1 s2 s3 s4 =
    Task.map4 (,,,)
        (function s1)
        (function s2)
        (function s3)
        (function s4)


firstRound seed s1 s2 s3 s4 =
    let
        tableName = getTableName seed
        declareGrandTichu (session, declare) =
            case declare of
                True ->
                    gamesWithSession session
                    |> postCommand (tableName ++ "/declareGrandTichu")
                False ->
                    gamesWithSession session
                    |> postCommand (tableName ++ "/seeAllCards")
    in
        execForAll declareGrandTichu (s1, False) (s2, False) (s3, False) (s4, False)
        |> andThenReturn
            (gamesWithSession s1
            |> get tableName)
        |> andThenReturn
            (playRound seed
                [ Play s4 [ MahJong ]
                ])
        |> Task.attempt (PlayRound "round1")


