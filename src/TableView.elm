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


component : String -> Component Model msg Msg
component name =
    Component (model name)
        update
        view
        (Just (init name))
        (Just subs)


-- MODEL


type alias Model =
    { name : String
    , awaitingTable : Maybe AwaitingTable
    , game : Maybe Game
    , lastFinishedTableUpdate : Maybe Time
    }


model : String -> Model
model name =
    Model name Nothing Nothing Nothing


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
                        { model
                            | lastFinishedTableUpdate = Nothing
                        }
                            ! [ case model.game of
                                    Just _ -> getTable model.name ctx
                                    _ -> init model.name ctx
                              ]
                    else
                        model ! []

                _ ->
                    model ! []

        CheckCard card ->
            model ! []
            --( updateGame game [ UpdatePlayer (checkCard card) ], Cmd.none )

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


view : ComponentView Model msg Msg
view ctx model =
    Page "Table" <|
        case model.game of
            Just game ->
                div []
                    [ node "style" [] [ text cssStyle ]
                    , gameView ctx game
                    ]
            Nothing ->
                case model.awaitingTable of
                    Just awaitingTable ->
                        div []
                            [ node "style" [] [ text cssStyle ]
                            , awaitingTableView ctx awaitingTable
                            ]
                    Nothing ->
                        div [] []


playerBox className table index =
    div [ class className ]
        [ case List.drop index table.users |> List.head of
            Just player ->
                text player.name
            _ ->
                text "awaiting for player"
        ]


awaitingTableView : Context msg Msg -> AwaitingTable -> Html msg
awaitingTableView ctx table =
    multiCellRow
        [ ( 2
          , [ div [ class "table-chat" ]
                [ div [ class "chat-header" ] [ text "Chat" ]
                , div [] [ text "fsdds" ]
                , div [] [ text "asdf" ]
                ]
            ]
          )
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
        , ( 2
          , [ div [ class "table-options" ]
                [ div [ class "table-options-header" ] [ text "Game" ]
                ]
            ]
          )
        ]


gameView : Context msg cMsg -> Game -> Html msg
gameView ctx game =
    multiCellRow
        [ ( 2
          , [ div [ class "table-chat" ]
                [ div [ class "chat-header" ] [ text "Chat" ]
                , div [] [ text "fsdds" ]
                , div [] [ text "asdf" ]
                ]
            ]
          )
        , ( 8, [ div [ class "table-main" ]
                [ playerBox "player-bottom" game 0
                , playerBox "player-right" game 1
                , playerBox "player-top" game 2
                , playerBox "player-left" game 3
                ]
               ] )
        , ( 2
          , [ div [ class "table-options" ]
                [ div [ class "table-options-header" ] [ text "Game" ]
                ]
            ]
          )
        ]


oldTichuView : Game -> Html Msg
oldTichuView game =
    div []
        --[ node "link" [ rel "stylesheet", href "https://bootswatch.com/darkly/bootstrap.css" ] []
        [ node "style" [] [ text cssStyle ]
        , div [ class "" ]
            (List.map printRow
                [ [ showRound game.round ]
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


printTableHand : List Card -> List (Html Msg)
printTableHand cards =
    printCards (List.sortWith cardOrder cards) []


showRound : Round -> Html Msg
showRound round =
    let
        table =
            List.map (\s -> div [] s)
                (List.map printTableHand round.table)

        players =
            List.map (showPlayer round.actualPlayer)
                (List.indexedMap (,) round.players)
    in
        div [] (table ++ players)


showPlayer : Int -> ( Int, Player ) -> Html Msg
showPlayer actualPlayer ( index, player ) =
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
            :: printCards player.hand player.selection



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
    left: 0px;
    top: 40%;
}
.player-right {
    position: absolute;
    display: inline-block;
    right: 0px;
    top: 40%;
}
.player-top {
    position: absolute;
    display: inline-block;
    top: 0px;
    left: 45%;
}
.player-bottom {
    position: absolute;
    display: inline-block;
    bottom: 0px;
    left: 45%;
}
.middle-table {
    position: absolute;
    display: inline-block;
    top: 45%;
    left: 45%;
}
"""


