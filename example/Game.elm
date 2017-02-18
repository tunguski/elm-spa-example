module Game exposing (..)


import Http exposing (Error(..))
import Process
import String
import Task exposing (..)
import Time exposing (second)


import AllBotGame exposing (..)
import ApiPartApi exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (games)
import MongoDb exposing (..)
import Rest exposing (..)
import Server exposing (..)
import SessionModel exposing (Session)
import TichuBot exposing (tableChanged)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import TichuLogic exposing (..)
import UserModel exposing (..)
import UrlParse exposing (..)


gamesApiPart :
    ApiPartApi msg
    -> Parse (Partial msg)
gamesApiPart api =
    P "games"
        [ S (\id ->
            [ F
                (\() ->
                    case api.request.method of
                        Get ->
                            getGame api id

                        _ ->
                            statusResponse 405 |> Result
                )
            , PF "seeAllCards" (\() -> seeAllCards api id)
            , PF "exchangeCards" (\() -> exchangeCards api id)
            , PF "declareTichu" (\() -> declareTichu api id)
            , PF "declareGrandTichu" (\() -> declareGrandTichu api id)
            , PF "pass" (\() -> pass api id)
            , PF "hand" (\() -> hand api id)
            , PF "giveDragon" (\() -> giveDragon api id)
            , PF "testGame" (\() -> testGame api id)
            ]
          )
        ]


testGame api name =
    allBotsGame api name 0
    |> Task.attempt (\result ->
        case result of
            Ok data ->
                api.sendResponse (okResponse <| toString data )
            Err error ->
              (error |> toString >> response 500 >> api.sendResponse)
    )
    |> Command


doWithTable : (Session -> Game -> Round -> Player -> Task Error Response) ->
              ApiPartApi msg -> String ->
              Partial msg
doWithTable function api id =
    api.doWithSession (\session ->
        get id games
        |> andThen (\table ->
            function session table table.round
                (getPlayer table.round session.username)
        )
    )


{-| Pass player's move

1. Check is it actual player
2. Check if player may pass - there is no requirement or player does not have that card
3. Update actual player
4. Save state

-}
pass : ApiPartApi msg -> String -> Partial msg
pass =
    doWithTable (\session table round player ->
        TichuLogic.pass session.username table round player
        |> processingResultToTask
    )


ifNothing maybe =
    case maybe of
        Just _ -> False
        Nothing -> True


{-| Exchange cards

1. Check is it start of round
2. Check if player did not exchange cards
3. Check if player did not
4. Update actual player
5. Save state

-}
exchangeCards : ApiPartApi msg -> String -> Partial msg
exchangeCards api =
    doWithTable (\session table round player ->
        TichuLogic.exchangeCards session.username table round player
            (decodeCards api.request.body)
        |> processingResultToTask
    ) api


{-| Place a hand on table

1. Check if it is actual player or played a bomb (higher if there was bomb before)
2. Place hand on table
3. Remove cards from player's hand
4. Update actual player
5. Save state

-}
hand : ApiPartApi msg -> String -> Partial msg
hand api =
    doWithTable (\session table round player ->
        case decodeCards api.request.body of
            Ok cards ->
                TichuLogic.hand
                    session.username
                    table
                    round
                    player
                    cards
                |> processingResultToTask
            _ ->
                response 400 "Malformed cards" |> Task.succeed
    ) api


{-| Declare tichu

1. Check if player may declare tichu - did not put any card in this round
2. Modify state

-}
declareTichu : ApiPartApi msg -> String -> Partial msg
declareTichu =
    updateAndReturnIf TichuLogic.declareTichu


{-| Declare grand tichu

1. Check if player may declare grand - saw only 8 cards
2. Modify state

-}
declareGrandTichu : ApiPartApi msg -> String -> Partial msg
declareGrandTichu =
    updateAndReturnIf TichuLogic.declareGrandTichu


{-| If player does not want to play grand tichu, he may see all cards

1. Check if player saw all cards
2. Modify state

-}
seeAllCards : ApiPartApi msg -> String -> Partial msg
seeAllCards =
    updateAndReturnIf TichuLogic.seeAllCards


{-| If condition is met (pass player to it), then
    execute update function on player.
-}
updateAndReturnIf exec =
    doWithTable (\session table round player ->
        exec session.username table round player
        |> processingResultToTask
    )


{-| Return awaiting table with id.
-}
getGame : ApiPartApi msg -> String -> Partial msg
getGame api id =
    api.doWithSession
        (\session ->
            get id games
            |> andThen
                (\table ->
                    put id
                        { table
                        | users = List.map (\user ->
                                        if user.name /= session.username then
                                            user
                                        else
                                            { user | lastCheck = api.request.time }
                                    )
                                    table.users
                        } games
                    |> andThenReturn (
                        let
                            round = table.round
                        in
                            ({ table
                             | round =
                                { round
                                | players = List.map (\player ->
                                        if player.name == session.username then
                                            if player.sawAllCards then
                                                player
                                            else
                                                { player
                                                | hand = List.take 8 player.hand
                                                }
                                        else
                                            { player
                                            | hand = []
                                            , exchange = Nothing
                                            }
                                    ) round.players
                                , seed = 0
                                }
                             , seed = 0
                             }
                            |> (encode gameEncoder >> okResponse >> Task.succeed)))
                )
            |> onError logErrorAndReturn
        )


giveDragon : ApiPartApi msg -> String -> Partial msg
giveDragon api =
    doWithTable (\session table round player ->
        TichuLogic.giveDragon session.username
            table round player (api.request.body == "next")
        |> processingResultToTask
    ) api


gamePostRequest : (Result String String -> msg) ->
                  String ->
                  Maybe (Cmd msg)
gamePostRequest m idTable =
    Process.sleep 1000
    |> andThen (\_ -> get idTable games)
    |> andThen (\table ->
        table.users
        |> List.indexedMap (,)
        |> List.filter (\(i, player) -> not player.human)
        |> List.map (\(i, player) ->
            Process.sleep (toFloat <| 250 * i)
            |> andThen (\_ -> get idTable games)
            |> andThen (
                tableChanged player.name
                >> Maybe.withDefault (Task.succeed "no move")
            )
        )
        |> Task.sequence
    )
    |> map (\result ->
        case result of
            [] ->
                ""
            _ ->
                Debug.log "postRequest" <| toString result
    )
    |> mapError toString
    |> attempt m
    |> Just


gamePostRequestPart msg request =
    P "games"
        [ S (\id ->
            [ F (\() -> Nothing)
            , PF "seeAllCards" (\() -> gamePostRequest msg id)
            , PF "exchangeCards" (\() -> gamePostRequest msg id)
            , PF "declareTichu" (\() -> gamePostRequest msg id)
            , PF "declareGrandTichu" (\() -> gamePostRequest msg id)
            , PF "pass" (\() -> gamePostRequest msg id)
            , PF "hand" (\() -> gamePostRequest msg id)
            , PF "giveDragon" (\() -> gamePostRequest msg id)
            , PF "testGame" (\() -> gamePostRequest msg id)
            ]
          )
        ]


