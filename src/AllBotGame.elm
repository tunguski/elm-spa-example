module AllBotGame exposing (allBotsGame)

import ApiPartApi exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (games)
import Http exposing (Error(..))
import MongoDb exposing (..)
import Rest exposing (..)
import Server exposing (..)
import SessionModel exposing (Session)
import String
import Task exposing (..)
import TichuLogic exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import Time 


allBotsGame : ApiPartApi msg -> String -> Int -> Task Error String
allBotsGame api name seed =
    put name
        (initGame name
            (GameConfig Bots)
            True
            seed
            [ AwaitingTableUser "bot 1" api.request.time True False
            , AwaitingTableUser "bot 2" api.request.time True False
            , AwaitingTableUser "bot 3" api.request.time True False
            , AwaitingTableUser "bot 4" api.request.time True False
            ]
        )
        games
        |> andThenReturn (succeed "")
