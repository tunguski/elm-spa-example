module Tests.PlayAGame exposing (Msg(..), PlayerRequest(..), awaitingTablesWithSession, errorResultToModel, execForAll, firstRound, gamesWithSession, getActualStates, getTableName, initPlayAGame, playFullRound, playRound, secondRound, update, view)

import ClientApi exposing (..)
import Component exposing (..)
import Config exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Rest exposing (..)
import SessionModel exposing (..)
import TableView exposing (gameView, oldTichuView)
import Task exposing (Task)
import TestBasics exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (encodeCards, encodeGameConfig)


getTableName seed =
    "playAGame" ++ Debug.toString seed


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
        tableName =
            getTableName seed

        getGuestToken =
            sessions
                |> withQueryParams
                    [ ( "noHeader", "true" )
                    , ( "forceNew", "true" )
                    , ( "seed", "0" )
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
    Task.map4 (\a b c d -> Quad a b c d)
        getGuestToken
        getGuestToken
        getGuestToken
        getGuestToken
        |> Task.andThen
            (\sessions ->
                -- ad. 2
                (awaitingTablesWithSession sessions.e1
                    |> withHeader "X-Test-Game-Seed" "0"
                    |> withBody (encodeGameConfig <| GameConfig Humans)
                    |> postCommand tableName
                )
                    |> andThenReturn
                        -- ad. 3
                        (execForAll joinTable sessions)
                    |> andThenReturn
                        -- ad. 4
                        (execForAll startTable sessions)
                    |> andThenReturn
                        -- ad. 5
                        (execForAll getTable sessions)
                    |> Task.andThen
                        (\games ->
                            Task.succeed ( sessions, games )
                        )
            )
        |> Task.attempt PlayAGameGetSession


getActualStates : String -> Quad Session -> Task Error (Quad Game)
getActualStates tableName sessions =
    let
        getTable session =
            gamesWithSession session
                |> get tableName
    in
    execForAll getTable sessions



-- UPDATE


type
    Msg
    -- play a game messages
    = PlayAGameGetSession (RestResult ( Quad Session, Quad Game ))
    | PlayRound String (RestResult (Quad Game))
    | Unused
    | TableStates (RestResult (Quad Game))


errorResultToModel seed model error =
    ( { model
        | result =
            Just <|
                Err <|
                    case error of
                        BadStatus response ->
                            Debug.toString ( response.status.code, response.body )

                        _ ->
                            Debug.toString error
      }
    , Cmd.batch
        (case model.sessions of
            Just sessions ->
                [ getActualStates (getTableName seed) sessions
                    |> Task.attempt TableStates
                ]

            Nothing ->
                []
        )
    )


update seed action model =
    case action of
        PlayAGameGetSession result ->
            case result of
                Ok ( sessions, games ) ->
                    ( { model
                        | sessions = Just sessions
                        , playerState = toList games
                      }
                    , firstRound seed sessions
                    )

                Err error ->
                    errorResultToModel seed model error

        PlayRound id result ->
            case result of
                Ok games ->
                    ( { model | playerState = toList games }
                        |> (\m ->
                                -- if that was last round and it finished property, test ended
                                if id == "round2" then
                                    { m | result = Just <| Ok "finished" }

                                else
                                    m
                           )
                    , Cmd.batch
                        (case model.sessions of
                            Just sessions ->
                                case id of
                                    "round1" ->
                                        [ secondRound seed sessions ]

                                    _ ->
                                        []

                            _ ->
                                []
                        )
                    )

                Err error ->
                    errorResultToModel seed model error

        Unused ->
            ( model
            , Cmd.none
            )

        TableStates result ->
            case result of
                Ok g ->
                    ( { model | playerState = toList g }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


type PlayerRequest
    = Pass Session
    | Play Session (List Card)
    | Tichu Session
    | GiveDragon Session String


playRound : Int -> List PlayerRequest -> Task Error (List String)
playRound seed requests =
    let
        tableName =
            getTableName seed
    in
    List.map
        (\r ->
            case r of
                Pass session ->
                    gamesWithSession session
                        |> postCommand (tableName ++ "/pass")

                Play session list ->
                    gamesWithSession session
                        |> withBody (encodeCards list)
                        |> postCommand (tableName ++ "/hand")

                Tichu session ->
                    gamesWithSession session
                        |> postCommand (tableName ++ "/declareTichu")

                GiveDragon session body ->
                    gamesWithSession session
                        |> withBody body
                        |> postCommand (tableName ++ "/giveDragon")
        )
        requests
        |> Task.sequence


execForAll function q =
    Task.map4 Quad
        (function q.e1)
        (function q.e2)
        (function q.e3)
        (function q.e4)


playFullRound seed name s g passing moves =
    let
        tableName =
            getTableName seed

        declareGrandTichu ( session, declare ) =
            case declare of
                True ->
                    gamesWithSession session
                        |> postCommand (tableName ++ "/declareGrandTichu")

                False ->
                    gamesWithSession session
                        |> postCommand (tableName ++ "/seeAllCards")

        exchangeCards ( cards, session ) =
            gamesWithSession session
                |> withBody (encodeCards cards)
                |> postCommand (tableName ++ "/exchangeCards")
    in
    execForAll declareGrandTichu (quadZip s g)
        |> andThenReturn (gamesWithSession s.e1 |> get tableName)
        |> andThenReturn
            (execForAll exchangeCards
                (quadZip passing s)
            )
        |> andThenReturn (playRound seed moves)
        |> andThenReturn (getActualStates tableName s)
        |> Task.attempt (PlayRound name)


firstRound seed s =
    playFullRound seed
        "round1"
        s
        (Quad False False False False)
        (Quad
            [ NormalCard Spades (R 2), Phoenix, NormalCard Spades (R 3) ]
            [ NormalCard Hearts (R 7), NormalCard Clubs A, NormalCard Clubs (R 8) ]
            [ NormalCard Hearts (R 3), NormalCard Spades A, NormalCard Diamonds (R 3) ]
            [ NormalCard Diamonds (R 5), Dragon, NormalCard Hearts (R 5) ]
        )
        [ Play s.e4 [ MahJong ]
        , Play s.e1 [ NormalCard Hearts (R 3) ]
        , Pass s.e2
        , Play s.e3 [ NormalCard Hearts (R 6) ]
        , Pass s.e4
        , Play s.e1 [ NormalCard Diamonds (R 7) ]
        , Play s.e2 [ NormalCard Diamonds (R 10) ]
        , Play s.e3 [ NormalCard Spades Q ]
        , Play s.e4 [ NormalCard Hearts A ]
        , Pass s.e1
        , Pass s.e2
        , Pass s.e3
        , Play s.e4 [ NormalCard Diamonds (R 3) ]
        , Play s.e1 [ NormalCard Spades (R 6) ]
        , Pass s.e2
        , Play s.e3 [ NormalCard Clubs (R 7) ]
        , Play s.e4 [ NormalCard Spades (R 8) ]
        , Play s.e1 [ NormalCard Clubs (R 9) ]
        , Pass s.e2
        , Pass s.e3
        , Play s.e4 [ NormalCard Hearts (R 10) ]
        , Play s.e1 [ NormalCard Hearts Q ]
        , Pass s.e2
        , Pass s.e3
        , Pass s.e4
        , Play s.e1
            [ NormalCard Diamonds (R 4)
            , NormalCard Spades (R 4)
            , NormalCard Hearts (R 5)
            , NormalCard Spades (R 5)
            , NormalCard Clubs (R 5)
            ]
        , Pass s.e2
        , Pass s.e3
        , Play s.e4
            [ NormalCard Clubs J
            , NormalCard Diamonds J
            , NormalCard Diamonds K
            , NormalCard Spades K
            , Phoenix
            ]
        , Pass s.e1
        , Pass s.e2
        , Pass s.e3
        , Play s.e4
            [ NormalCard Clubs (R 6)
            , NormalCard Diamonds (R 6)
            , NormalCard Hearts (R 7)
            , NormalCard Spades (R 7)
            ]
        , Pass s.e1
        , Pass s.e2
        , Pass s.e3
        , Play s.e1 [ Dog ]
        , Play s.e3 [ NormalCard Clubs (R 3) ]
        , Play s.e1 [ NormalCard Clubs A ]
        , Pass s.e2
        , Pass s.e3
        , Play s.e1 [ NormalCard Diamonds (R 8), NormalCard Hearts (R 8) ]
        , Play s.e2 [ NormalCard Diamonds (R 9), NormalCard Spades (R 9) ]
        , Pass s.e3
        , Play s.e2
            [ NormalCard Clubs (R 10)
            , NormalCard Spades (R 10)
            , NormalCard Hearts J
            , NormalCard Spades J
            , NormalCard Diamonds Q
            , NormalCard Clubs Q
            , NormalCard Hearts K
            , NormalCard Clubs K
            ]
        , Play s.e3
            [ NormalCard Clubs (R 2)
            , NormalCard Diamonds (R 2)
            , NormalCard Hearts (R 2)
            , NormalCard Spades (R 2)
            ]
        , Pass s.e2
        , Play s.e3 [ NormalCard Clubs (R 8) ]
        , Play s.e2 [ NormalCard Spades A ]
        , Play s.e3 [ Dragon ]
        , Pass s.e2
        , Play s.e3 [ NormalCard Hearts (R 9) ]
        , Pass s.e2
        , Play s.e3 [ NormalCard Diamonds A ]
        , Pass s.e2
        , Play s.e3 [ NormalCard Hearts (R 4), NormalCard Clubs (R 4) ]
        ]


secondRound seed s =
    playFullRound seed
        "round2"
        s
        (Quad False False False False)
        (Quad
            [ NormalCard Diamonds (R 2), NormalCard Hearts Q, NormalCard Spades (R 9) ]
            [ NormalCard Clubs (R 5), NormalCard Hearts K, NormalCard Diamonds (R 8) ]
            [ NormalCard Spades (R 2), NormalCard Clubs (R 9), NormalCard Spades (R 8) ]
            [ NormalCard Hearts (R 2), NormalCard Spades Q, NormalCard Hearts (R 4) ]
        )
        [ Play s.e2
            [ MahJong
            , NormalCard Hearts (R 2)
            , NormalCard Diamonds (R 3)
            , NormalCard Clubs (R 4)
            , NormalCard Spades (R 5)
            , NormalCard Spades (R 6)
            , NormalCard Spades (R 7)
            , NormalCard Hearts (R 8)
            ]
        , Pass s.e3
        , Pass s.e4
        , Pass s.e1
        , Play s.e2 [ Dog ]
        , Tichu s.e4
        , Play s.e4 [ NormalCard Spades (R 4) ]
        , Play s.e1 [ NormalCard Diamonds (R 5) ]
        , Pass s.e2
        , Tichu s.e3
        , Play s.e3 [ NormalCard Diamonds (R 8) ]
        , Play s.e4 [ NormalCard Hearts Q ]
        , Pass s.e1
        , Pass s.e2
        , Play s.e3 [ NormalCard Hearts A ]
        , Pass s.e4
        , Pass s.e1
        , Pass s.e2
        , Play s.e3 [ NormalCard Diamonds (R 2) ]
        , Play s.e4 [ NormalCard Hearts (R 7) ]
        , Play s.e1 [ NormalCard Clubs (R 10) ]
        , Pass s.e2
        , Play s.e3 [ NormalCard Diamonds A ]
        , Play s.e4 [ Phoenix ]
        , Pass s.e1
        , Pass s.e2
        , Play s.e3 [ Dragon ]
        , Pass s.e4
        , Pass s.e1
        , Pass s.e2
        , GiveDragon s.e3 "next"
        , Play s.e3
            [ NormalCard Clubs (R 3)
            , NormalCard Spades (R 3)
            , NormalCard Diamonds J
            , NormalCard Hearts J
            , NormalCard Spades J
            ]
        , Pass s.e4
        , Pass s.e1
        , Pass s.e2
        , Play s.e3
            [ NormalCard Diamonds Q
            , NormalCard Clubs Q
            , NormalCard Spades Q
            ]
        , Pass s.e4
        , Pass s.e1
        , Pass s.e2
        , Play s.e3 [ NormalCard Diamonds (R 10) ]
        , Pass s.e4
        , Pass s.e1
        , Pass s.e2
        , Play s.e4
            [ NormalCard Clubs (R 8)
            , NormalCard Spades (R 8)
            , NormalCard Hearts (R 9)
            , NormalCard Diamonds (R 9)
            , NormalCard Hearts (R 10)
            , NormalCard Spades (R 10)
            ]
        , Pass s.e1
        , Pass s.e2
        , Play s.e4 [ NormalCard Hearts (R 5), NormalCard Clubs (R 5) ]
        , Pass s.e1
        , Play s.e2 [ NormalCard Clubs (R 7), NormalCard Diamonds (R 7) ]
        , Play s.e4 [ NormalCard Spades A, NormalCard Clubs A ]
        , Pass s.e1
        , Pass s.e2
        , Play s.e1
            [ NormalCard Hearts (R 4)
            , NormalCard Diamonds (R 4)
            , NormalCard Hearts (R 6)
            , NormalCard Diamonds (R 6)
            , NormalCard Clubs (R 6)
            ]
        , Pass s.e2
        , Play s.e1 [ NormalCard Hearts (R 3) ]
        , Play s.e2 [ NormalCard Clubs J ]
        , Play s.e1
            [ NormalCard Hearts K
            , NormalCard Diamonds K
            , NormalCard Spades K
            , NormalCard Clubs K
            ]
        , Pass s.e2
        , Play s.e1 [ NormalCard Clubs (R 2), NormalCard Spades (R 2) ]
        ]


view ctx model =
    [ maybeTestHeader "Play a game" (maybeResultSuccess model.result)
    , div []
        (List.map
            (\table ->
                div [ class "col-md-6" ]
                    [ gameView (Context (always <| ctx.mapMsg Unused)) "testUser" (TableView.emptyModel "testUser" "testTable") table ]
            )
            model.playerState
            ++ List.map
                (\table ->
                    div [ class "col-md-6" ]
                        [ Html.map (always <| ctx.mapMsg Unused) (oldTichuView [] table) ]
                )
                model.playerState
            ++ List.map
                (\table ->
                    div [ class "col-md-3" ]
                        (List.indexedMap
                            (\i round ->
                                div []
                                    (div [ class "col-md-offset-2 col-md-2" ] [ text <| (Debug.toString <| i + 1) ++ ". " ]
                                        :: List.map
                                            (\playerPoints ->
                                                div [ class "col-md-2" ] [ text <| Debug.toString playerPoints ]
                                            )
                                            (calculatePlayersPoints round)
                                    )
                            )
                            table.history
                            |> List.reverse
                        )
                )
                model.playerState
        )
    , displayResult model.result
    ]
