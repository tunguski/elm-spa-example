module Tichu.Update exposing (..)

import Tichu.Model exposing (..)
import List exposing (..)
import Http exposing (..)
import Task exposing (..)
import Dict exposing (Dict, empty, insert, update)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task


-----------------------------------------------------------------------------
-- UPDATE
-----------------------------------------------------------------------------


type Msg
    = CheckCard Card
    | DeclareTichu Player
    | DeclareGrandTichu Player
    | PlaceCombination
    | MorePlease
    | FetchSucceed String
    | FetchFail Http.Error


update : Msg -> Game -> ( Game, Cmd Msg )
update action game =
    case action of
        CheckCard card ->
            ( updateGame game [ UpdatePlayer (checkCard card) ], Cmd.none )

        DeclareTichu player ->
            ( updateGame game [ UpdatePlayer declareTichu ], Cmd.none )

        DeclareGrandTichu player ->
            ( updateGame game [ UpdatePlayer declareGrandTichu ], Cmd.none )

        PlaceCombination ->
            ( updateGame game [ UpdateRound placeCombination ], Cmd.none )

        MorePlease ->
            ( game, getRandomGif "cat" )

        FetchSucceed data ->
            ( game, Cmd.none )

        --        ({ game | log = data :: game.log }, Cmd.none)
        FetchFail error ->
            ( game, Cmd.none )



--        ({ game | log = error :: game.log }, Cmd.none)
-- RANDOM GIF


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "//api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
        Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)


decodeGifUrl : Json.Decoder String
decodeGifUrl =
    Json.at [ "data", "image_url" ] Json.string



-- RANDOM GIF END


checkCard : Card -> Player -> Player
checkCard card player =
    if member card player.hand then
        if member card player.selection then
            { player | selection = filter (\c -> card /= c) player.selection }
        else
            { player | selection = card :: player.selection }
    else
        player


declareTichu : Player -> Player
declareTichu player =
    { player | tichu = True }


declareGrandTichu : Player -> Player
declareGrandTichu player =
    { player | grandTichu = True }


removeSelectionFromHand : Player -> Player
removeSelectionFromHand player =
    { player
        | hand = filter (\c -> not <| member c player.selection) player.hand
        , selection = []
    }


placeCombination : Round -> Round
placeCombination round =
    let
        selection =
            Maybe.map (\p -> p.selection) (Dict.get round.actualPlayer round.players)

        updatedPlayers =
            Dict.update round.actualPlayer (Maybe.map removeSelectionFromHand) round.players
    in
        { round
            | players = updatedPlayers
            , actualPlayer = ((round.actualPlayer + 1) % 4)
            , table = (sortWith cardOrder <| Maybe.withDefault [] selection) :: round.table
        }


updateRound : Round -> Round
updateRound round =
    round


updateGame : Game -> List UpdateGame -> Game
updateGame game updates =
    foldl executeUpdate game updates


updatePlayersDict : (Player -> Player) -> Round -> Round
updatePlayersDict playerUpdate round =
    { round | players = Dict.update round.actualPlayer (Maybe.map playerUpdate) round.players }


executeUpdate : UpdateGame -> Game -> Game
executeUpdate update game =
    case update of
        UpdatePlayer playerUpdate ->
            { game
                | round = updatePlayersDict playerUpdate game.round
                , log = update :: game.log
            }

        UpdateRound roundUpdate ->
            { game
                | round = roundUpdate game.round
                , log = update :: game.log
            }


updateFirst : (a -> a) -> List a -> List a
updateFirst updater list =
    case list of
        [] ->
            list

        head :: tail ->
            (updater head) :: tail
