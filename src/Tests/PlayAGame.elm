module Tests.PlayAGame exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http exposing (Error(..))
import Task exposing (Task)


import ClientApi exposing (..)
import Config exposing (..)
import Component exposing (..)
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


getActualStates tableName sessions =
    let
        getTable session =
            gamesWithSession session
            |> get tableName
    in
        (execForAll getTable sessions)

-- UPDATE


type Msg
    -- play a game messages
    = PlayAGameGetSession (RestResult
            ( Quad Session
            , Quad Game
            )
        )
    | PlayRound String (RestResult (Quad Game))
    | Unused
    | TableStates (RestResult (Quad Game))


errorResultToModel seed model error =
    { model | result =
        Just <| Err <|
            case error of
                BadStatus response ->
                    toString (response.status.code, response.body)
                _ ->
                    toString error
    }
    ! case model.sessions of
            Just sessions ->
                [ getActualStates (getTableName seed) sessions
                    |> Task.attempt TableStates
                ]
            Nothing ->
                []


update seed action model =
    case action of
        PlayAGameGetSession result ->
           case result of
               Ok (sessions, games) ->
                   { model
                   | sessions = Just sessions
                   , playerState = toList games
                   }
                   ! [ firstRound seed sessions ]
               Err error ->
                    errorResultToModel seed model error

        PlayRound id result ->
            case result of
                Ok games ->
                    { model
                    | result = Just <| Ok "finished"
                    , playerState = toList games
                    } ! []
                Err error ->
                    errorResultToModel seed model error

        Unused ->
            model ! []

        TableStates result ->
            case result of
                Ok (g1, g2, g3, g4) ->
                    { model | playerState = [ g1, g2, g3, g4 ] } ! []
                _ ->
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


firstRound seed (s1, s2, s3, s4) =
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
            (gamesWithSession s1 |> get tableName)
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
                , Pass s3
                , Play s4 [ NormalCard Diamonds (R 3) ]
                , Play s1 [ NormalCard Spades (R 6) ]
                , Pass s2
                , Play s3 [ NormalCard Clubs (R 7) ]
                , Play s4 [ NormalCard Spades (R 8) ]
                , Play s1 [ NormalCard Clubs (R 9) ]
                , Pass s2
                , Pass s3
                , Play s4 [ NormalCard Hearts (R 10) ]
                , Play s1 [ NormalCard Hearts Q ]
                , Pass s2
                , Pass s3
                , Pass s4
                , Play s1
                    [ NormalCard Diamonds (R 4)
                    , NormalCard Spades (R 4)
                    , NormalCard Hearts (R 5)
                    , NormalCard Spades (R 5)
                    , NormalCard Clubs (R 5)
                    ]
                , Pass s2
                , Pass s3
                , Play s4
                    [ NormalCard Clubs J
                    , NormalCard Diamonds J
                    , NormalCard Diamonds K
                    , NormalCard Spades K
                    , Phoenix
                    ]
                , Pass s1
                , Pass s2
                , Pass s3
                , Play s4
                    [ NormalCard Clubs (R 6)
                    , NormalCard Diamonds (R 6)
                    , NormalCard Hearts (R 7)
                    , NormalCard Spades (R 7)
                    ]
                , Pass s1
                , Pass s2
                , Pass s3
                , Play s1 [ Dog ]
                , Play s3 [ NormalCard Clubs (R 3) ]
                , Play s1 [ NormalCard Clubs A ]
                , Pass s2
                , Pass s3
                , Play s1 [ NormalCard Diamonds (R 8), NormalCard Hearts (R 8) ]
                , Play s2 [ NormalCard Diamonds (R 9), NormalCard Spades (R 9) ]
                , Pass s3
                , Play s2 [ NormalCard Clubs (R 10), NormalCard Spades (R 10)
                          , NormalCard Hearts J, NormalCard Spades J
                          , NormalCard Diamonds Q, NormalCard Clubs Q
                          , NormalCard Hearts K, NormalCard Clubs K
                          ]
                , Play s3 [ NormalCard Clubs (R 2)
                          , NormalCard Diamonds (R 2)
                          , NormalCard Hearts (R 2)
                          , NormalCard Spades (R 2)
                          ]
                , Pass s2
                , Play s3 [ NormalCard Clubs (R 8) ]
                , Play s2 [ NormalCard Spades A ]
                , Play s3 [ Dragon ]
                , Pass s2
                , Play s3 [ NormalCard Hearts (R 9) ]
                , Pass s2
                , Play s3 [ NormalCard Diamonds A ]
                , Pass s2
                , Play s3 [ NormalCard Hearts (R 4), NormalCard Clubs (R 4) ]
                ])
        |> andThenReturn
            (getActualStates tableName (s1, s2, s3, s4))
        |> Task.attempt (PlayRound "round1")


view ctx model =
    [ maybeTestHeader "Play a game" (maybeResultSuccess model.result)
    , div []
      ((List.map (\table ->
          div [ class "col-md-6" ]
            [ gameView ctx table ]
        ) model.playerState)
      ++
      (List.map (\table ->
          div [ class "col-md-6" ]
            [ Html.map (always <| ctx.mapMsg Unused) (oldTichuView table) ]
        ) model.playerState)
      ++
      (List.map (\table ->
          div []
            (List.map (\round ->
                  div [ class "col-md-3" ]
                      (List.map (\playerPoints ->
                          div [ class "col-md-3" ] [ text <| toString playerPoints ])
                          (calculatePlayersPoints round)))
                  table.history)
        ) model.playerState)
      )
    , displayResult model.result
    ]


