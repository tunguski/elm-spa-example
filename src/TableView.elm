module TableView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Navigation
import Json.Decode as Json exposing (..)
import Result exposing (toMaybe)
import Task exposing (Task)
import Time exposing (Time, every, second, now)


import ClientApi exposing (..)
import Config exposing (..)
import Component exposing (..)
import BaseModel exposing (..)
import Rest exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (..)


component : String -> String -> Component Model msg Msg
component userName tableName =
    Component (model userName tableName)
        update
        view
        (Just (init tableName))
        (Just subs)


-- MODEL


type alias Model =
    { userName : String
    , name : String
    , awaitingTable : Maybe AwaitingTable
    , game : Maybe Game
    , selection : List Card
    , lastFinishedTableUpdate : Maybe Time
    }


model : String -> String -> Model
model userName tableName =
    Model userName tableName Nothing Nothing [] Nothing


init : String -> Context msg Msg -> Cmd msg
init name ctx =
    Task.map2
        (,)
        now
        ((get name awaitingTables
            |> Task.map Ok
         )
            |> Task.onError
                (\error ->
                    get name games
                    |> Task.map Err
                )
        )
    |> Task.attempt UpdateTables
    |> Cmd.map ctx.mapMsg


getTable : String -> Context msg Msg -> Cmd msg
getTable name ctx =
    Task.map2
        (,)
        now
        (get name games |> Task.map Err)
    |> Task.attempt UpdateTables
    |> Cmd.map ctx.mapMsg


startGame : String -> (RestResult String -> msg) -> Cmd msg
startGame name msg =
    postCommand (name ++ "/start") awaitingTables
    |> Task.attempt msg


subs : Context msg Msg -> model -> Sub msg
subs ctx model =
    every second CheckUpdate
        |> Sub.map ctx.mapMsg


-- UPDATE


type Msg
    = UpdateTables (RestResult (Time, Result Game AwaitingTable))
    | CheckUpdate Time
    | CheckCard Card
    | DeclareTichu Player
    | DeclareGrandTichu Player
    | PlaceCombination
    | Start
    | SentStart (RestResult String)


update : ComponentUpdate Model msg Msg
update ctx action model =
    case action of
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
                        { model | lastFinishedTableUpdate = Nothing }
                            ! [ case model.game of
                                    Just _ -> getTable model.name ctx
                                    _ -> init model.name ctx
                              ]
                    else
                        model ! []

                _ ->
                    model ! []

        CheckCard card ->
            case List.member card model.selection of
                True ->
                    { model | selection = List.filter ((/=) card) model.selection } ! []
                False ->
                    { model | selection = card :: model.selection } ! []

        DeclareTichu player ->
            model ! []
            --( updateGame game [ UpdatePlayer declareTichu ], Cmd.none )

        DeclareGrandTichu player ->
            model ! []
            --( updateGame game [ UpdatePlayer declareGrandTichu ], Cmd.none )

        PlaceCombination ->
            model ! []
            --( updateGame game [ UpdateRound placeCombination ], Cmd.none )

        Start ->
            model ! case model.awaitingTable of
                        Just t -> [ startGame t.name <| ctx.mapMsg << SentStart ]
                        _ -> []

        SentStart result ->
            model ! []


-- VIEW


orElse maybe generator =
    case maybe of
        Nothing -> generator
        _ -> maybe


view : ComponentView Model msg Msg
view ctx model =
    Page "Table" <|
        div [] <|
            [ node "style" [] [ text cssStyle ] ]
            ++
            case model.game of
                Just game ->
                        [ gameView ctx model.userName model.selection game ]
                Nothing ->
                    case model.awaitingTable of
                        Just awaitingTable ->
                            [ awaitingTableView ctx awaitingTable ]
                        Nothing ->
                            []


playerBox className table index =
    div [ class className ]
        [ case List.drop index table.users |> List.head of
            Just player ->
                text player.name
            _ ->
                text "awaiting for player"
        ]


playerGameBox : List Card -> String -> Game -> Int -> Html Msg
playerGameBox selection className game index =
    case List.drop index game.round.players |> List.head of
        Just player ->
            div [ class className ] <|
                printTableHand selection player.hand
                ++
                [ text player.name
                ]
        _ ->
            div [ class className ] [ text "error" ]


chatPanel chatData =
    ( 2
    , [ div [ class "table-chat" ]
          [ div [ class "chat-header" ] [ text "Chat" ]
          , div [] [ text "fsdds" ]
          , div [] [ text "asdf" ]
          ]
      ]
    )


gameSummaryPanel gameSummary =
    ( 2
    , [ div [ class "table-options" ]
          [ div [ class "table-options-header" ] [ text "Game" ]
          ]
      ]
    )


awaitingTableView : Context msg Msg -> AwaitingTable -> Html msg
awaitingTableView ctx table =
    multiCellRow
        [ chatPanel ()
        , ( 8, [ div [ class "table-main" ]
                [ playerBox "player-bottom" table 0
                , playerBox "player-right" table 1
                , playerBox "player-top" table 2
                , playerBox "player-left" table 3
                , List.head table.users
                  |> Maybe.map (.pressedStart >> not >> (&&) (List.length table.users == 4))
                  |> Maybe.andThen (\show ->
                      case show of
                          True ->
                              Just <|
                                div
                                  [ class "btn btn-primary middle-table"
                                  , onClick <| ctx.mapMsg Start
                                  ]
                                  [ text "Start" ]
                          _ ->
                              Nothing
                  )
                  |> Maybe.withDefault (div [] [])
                ]
               ] )
        , gameSummaryPanel ()
        ]


grandTichuButton game player =
    case player.sawAllCards of
        True -> div [] []
        False -> button [ class "btn btn-primary" ] [ text "Grand Tichu" ]


tichuButton game player =
    case player.sawAllCards of
        True -> div [] []
        False -> button [ class "btn btn-primary" ] [ text "Tichu" ]


playButton game player =
    case player.sawAllCards of
        True -> div [] []
        False -> button [ class "btn btn-primary" ] [ text "Play" ]


passButton game player =
    case player.sawAllCards of
        True -> div [] []
        False -> button [ class "btn btn-primary" ] [ text "Pass" ]


gameView : Context msg Msg -> String -> List Card -> Game -> Html msg
gameView ctx userName selection game =
    List.filter (.name >> (==) userName) game.round.players
    |> List.head
    |> Maybe.map (\player ->
        multiCellRow
            [ chatPanel game
            , ( 8, [ Html.map ctx.mapMsg <|
                        div [ class "table-main" ]
                            [ playerGameBox selection "player-bottom" game 0
                            , playerGameBox selection "player-right" game 1
                            , playerGameBox selection "player-top" game 2
                            , playerGameBox selection "player-left" game 3
                            , div [ class "game-buttons" ]
                                [ grandTichuButton game player
                                , tichuButton game player
                                , playButton game player
                                , passButton game player
                                ]
                            ]
                   ] )
            , gameSummaryPanel game
            ]
        )
    |> Maybe.withDefault (div [] [ text "" ])


oldTichuView : List Card -> Game -> Html Msg
oldTichuView selection game =
    div []
        --[ node "link" [ rel "stylesheet", href "https://bootswatch.com/darkly/bootstrap.css" ] []
        [ node "style" [] [ text cssStyle ]
        , div [ class "" ]
            (List.map printRow
                [ [ showRound selection game.round ]
                , [ button
                        [ class "btn btn-sm btn-primary"
                        , onClick PlaceCombination
                        ]
                        [ text "Place" ]
                  ]
                  --        , h3 [] [ text "Logs" ] :: map (\l -> div [ class "text-muted" ] [ text (toString l) ]) game.log
                ]
            )
        ]


printRow : List (Html Msg) -> Html Msg
printRow content =
    div [ class "row" ] [ div [ class "col-md-12" ] content ]


printCards : List Card -> List Card -> List (Html Msg)
printCards cards selection =
    List.map (printCard selection) (List.sortWith cardOrder cards)


printCard : List Card -> Card -> Html Msg
printCard selection card =
    div
        [ class <|
            String.join " "
                [ "card-outer"
                , "selected-" ++ (toString (List.member card selection))
                ]
        , onClick (CheckCard card)
        ]
        [ printCardSkeleton card ]


printCardSkeleton : Card -> Html Msg
printCardSkeleton card =
    case card of
        NormalCard suit rank ->
            div [ class <| "suit-mark " ++ (toString suit |> String.toLower) ]
                [ text <|
                    case rank of
                        R i ->
                            toString i

                        r ->
                            toString r
                ]

        -- special cards
        a ->
            text (toString a)


printTableHand : List Card -> List Card -> List (Html Msg)
printTableHand selection cards =
    printCards (List.sortWith cardOrder cards) selection



showRound : List Card -> Round -> Html Msg
showRound selection round =
    let
        table =
            List.map (\s -> div [] s)
                (List.map (printTableHand selection) round.table)

        players =
            List.map (showPlayer selection round.actualPlayer)
                (List.indexedMap (,) round.players)
    in
        div [] (table ++ players)


showPlayer : List Card -> Int -> ( Int, Player ) -> Html Msg
showPlayer selection actualPlayer ( index, player ) =
    div [] <|
        (div
            [ class
                (""
                    ++ (if actualPlayer == index then
                            "text-success"
                        else
                            ""
                       )
                )
            ]
            [ text (player.name ++ " " ++ toString player.cardsOnHand) ]
        )
            :: printCards player.hand selection



-----------------------------------------------------------------------------
-- CSS STYLES
-----------------------------------------------------------------------------


cssStyle : String
cssStyle =
    """
.card-outer {
  border: 1px solid grey;
  border-radius: 7px;
  display: inline-block;
  margin: 5px;
  padding: 3px 5px;
}
.card-outer:hover {
  border-color: blue;
  background-color: #777;
  cursor: pointer;
}
.selected-True {
  background-color: #555;
}
.suit-mark {
}
.suit-mark:after {
  margin-left: 0.3em;
}
.spades:after {
  content: '♠';
}
.hearts:after {
  content: '♥';
  color: green;
}
.diamonds:after {
  content: '♦';
  color: red;
}
.clubs:after {
  content: '♣';
  color: blue;
}
.table-main {
    position: relative;
}
.player-left {
    position: absolute;
    display: inline-block;
    left: 10px;
    top: 40%;
}
.player-right {
    position: absolute;
    display: inline-block;
    right: 10px;
    top: 40%;
}
.player-top {
    position: absolute;
    display: inline-block;
    top: 10px;
    left: 45%;
}
.player-bottom {
    position: absolute;
    display: inline-block;
    bottom: 10px;
    left: 45%;
}
.middle-table {
    position: absolute;
    display: inline-block;
    top: 45%;
    left: 45%;
}
.game-buttons {
    position: absolute;
    display: inline-block;
    left: 10px;
    bottom: 10px;
}
"""


