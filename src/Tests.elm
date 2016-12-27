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
        (initialGame "TestGame"
            |> encodeGame
            |> decodeString gameDecoder
        )
        (AwaitingTable "AwaitingTable" [ ( "player one", 0 ) ]
            |> encodeAwaitingTable
            |> decodeString awaitingTableDecoder
        )
        (GameState Array.empty [] Nothing (initialGame "test"))


init : Context msg Msg -> Cmd msg
init ctx =
--  Random.generate BaseRandom (Random.int 0 Random.maxInt)
--    |> Cmd.map ctx.mapMsg
    Cmd.batch
        [ Random.generate BaseRandom (Random.int 0 Random.maxInt)
          |> Cmd.map ctx.mapMsg
        , initPlayAGame
          |> Cmd.map ctx.mapMsg
        ]


-- 1. create four guest players; remember their tokens
-- 2. create table by first of them; pass 'test-seed=<constant seed>' header
-- 3. join by the rest
-- 4. all press start
-- 5. play the game (how?)
initPlayAGame =
    let
        getGuestToken =
            sessions
                |> withQueryParams [ ("noHeader", "true"), ("forceNew", "true") ]
                |> get "guest"
    in
        Task.map4 (,,,)
            getGuestToken
            getGuestToken
            getGuestToken
            getGuestToken
        |> Task.attempt PlayAGameGetSession
--        postCommand (toString model.seed) awaitingTables
--            |> andThen (\_ -> get (toString model.seed) awaitingTables)
--            |> andThen (\_ -> get (toString model.seed) awaitingTables)


-- UPDATE


type Msg
    = BaseRandom Int
    | UpdateTables (RestResult ( Time, Result Game AwaitingTable ))
    | CheckUpdate Time
    | PlayAGameGetSession (RestResult (Session, Session, Session, Session))


update : ComponentUpdate Model msg Msg
update ctx action model =
    case action of
        PlayAGameGetSession result ->
            let
                playAGame = model.playAGame
            in
                case result of
                    Ok (s1, s2, s3, s4) ->
                        { model 
                        | playAGame = { playAGame | sessions = Array.fromList [ s1, s2, s3, s4 ] }
                        } ! []
                    Err _ ->
                        { model | seed = model.seed } ! []

        BaseRandom int ->
            { model | seed = int } ! []

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
    h4 []
        [ text
            (case passed of
                Just p ->
                    ((if p then
                        "[SUCC] "
                      else
                        "[FAIL] "
                     )
                        ++ name
                    )

                Nothing ->
                    ("[....] " ++ name)
            )
        ]


maybeResultSuccess result =
    Maybe.map resultSuccess result


testHeader name passed =
    h4 []
        [ text
            ((if passed then
                "[SUCC] "
              else
                "[FAIL] "
             )
                ++ name
            )
        ]


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
