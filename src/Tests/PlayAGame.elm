module Tests.PlayAGame exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http exposing (Error)
import Task exposing (Task)


import ClientApi exposing (..)
import Config exposing (..)
import Rest exposing (..)
import SessionModel exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (encodeCards)
import TestBasics exposing (..)
import TableView exposing (gameView, oldTichuView)


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
        |> Task.andThen (\sessions ->
            -- ad. 2
            (awaitingTablesWithSession (quadGet 1 sessions)
             |> withHeader "X-Test-Game-Seed" "0"
             |> postCommand tableName)
            |> andThenReturn
                -- ad. 3
                (execForAll joinTable sessions)
            |> andThenReturn
                -- ad. 4
                (execForAll startTable sessions)
            |> andThenReturn
                -- ad. 5
                (execForAll getTable sessions)
            |> Task.andThen (\games ->
                Task.succeed (sessions, games)
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
           case result of
               Ok ((s1, s2, s3, s4), (t1, t2, t3, t4)) ->
                   { model
                   | sessions = [ s1, s2, s3, s4 ]
                   , playerState = [ t1, t2, t3, t4 ]
                   }
                   ! [ firstRound seed s1 s2 s3 s4 ]
               Err error ->
                    { model | result = Just <| Err <| toString error } ! []

        PlayRound id result ->
        --    model ! []
            case result of
                Ok _ ->
                    { model | result = Just <| Ok "finished" } ! []
                Err error ->
                    { model | result = Just <| Err <| toString error } ! []


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
                    |> withBody (encodeCards list)
                    |> postCommand (tableName ++ "/hand")
        ) requests
        |> Task.sequence


execForAll function (s1, s2, s3, s4) =
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
        exchangeCards (cards, session) =
             gamesWithSession session
             |> withBody (encodeCards cards)
             |> postCommand (tableName ++ "/exchangeCards")
    in
        execForAll declareGrandTichu (quadMap (\s -> (s, False)) (s1, s2, s3, s4))
        |> andThenReturn
            (gamesWithSession s1
            |> get tableName)
        |> andThenReturn
            (execForAll exchangeCards (quadZip
                ( [ (NormalCard Spades (R 2)), Phoenix, (NormalCard Spades (R 3)) ]
                , [ (NormalCard Hearts (R 7)), (NormalCard Clubs A), (NormalCard Clubs (R 8)) ]
                , [ (NormalCard Hearts (R 3)), (NormalCard Spades A), (NormalCard Diamonds (R 3)) ]
                , [ (NormalCard Diamonds (R 5)), Dragon, (NormalCard Hearts (R 5)) ]
                )
                (s1, s2, s3, s4)))
        |> andThenReturn
            (playRound seed
                [ Play s4 [ MahJong ]
                , Play s1 [ NormalCard Hearts (R 3) ]
                , Pass s2
                , Play s3 [ NormalCard Hearts (R 6) ]
                , Pass s4
                , Play s1 [ NormalCard Diamonds (R 7) ]
                , Play s2 [ NormalCard Diamonds (R 10) ]
                , Play s3 [ NormalCard Spades Q ]
                , Play s4 [ NormalCard Hearts A ]
                , Pass s1
                , Pass s2
                , Play s3 [ NormalCard Clubs (R 4), NormalCard Hearts (R 4) ]
                ])
        |> Task.attempt (PlayRound "round1")


view ctx model =
    [ maybeTestHeader "Play a game" (maybeResultSuccess model.result)
    , div []
      ((List.map (\table ->
          div [ class "col-md-3" ]
            [ gameView ctx table ]
        ) model.playerState)
      ++
      (List.map (\table ->
          div [ class "col-md-3" ]
            [ oldTichuView table ]
        ) model.playerState
        ))
    , displayResult model.result
    ]


