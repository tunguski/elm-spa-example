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


getTable : String -> (RestResult Game -> msg) -> Cmd msg
getTable tableId msg =
    get tableId games
    |> Task.attempt msg


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
    | MorePlease


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
                            ! [ init model.name ctx ]
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

        MorePlease ->
            model ! []
            --( game, getRandomGif "cat" )


-- VIEW


view : ComponentView Model msg Msg
view ctx model =
    Page "Table" <|
        case model.game of
            Just game ->
                gameView ctx game
            Nothing ->
                div [] []


gameView : Context msg cMsg -> Game -> Html msg
gameView ctx model =
    multiCellRow
        [ ( 2
          , [ div [ class "table-chat" ]
                [ div [ class "chat-header" ] [ text "Chat" ]
                , div [] [ text "fsdds" ]
                , div [] [ text "asdf" ]
                ]
            ]
          )
        , ( 8, [ div [ class "table-main" ] [ text "main" ] ] )
        , ( 2
          , [ div [ class "table-options" ]
                [ div [ class "table-options-header" ] [ text "Game" ]
                ]
            ]
          )
        ]


oldTichuView : Context msg cMsg -> Game -> Html msg
oldTichuView ctx game =
    div []
        --[ node "link" [ rel "stylesheet", href "https://bootswatch.com/darkly/bootstrap.css" ] []
        [ node "style" [] [ text (cssStyle game) ]
        , div [ class "container" ]
            (List.map printRow
                [ [ h1 [] [ text "Test" ] ]
                , [ showRound game.round ]
                , [ button
                        [ class "btn btn-sm btn-primary"
                        , onClick PlaceCombination
                        ]
                        [ text "Place" ]
                  ]
                , [ button
                        [ class "btn btn-sm btn-info"
                        , onClick MorePlease
                        ]
                        [ text "More Please" ]
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
    List.map (printCard selection) cards


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
    List.map (printCard []) cards


showRound : Round -> Html Msg
showRound round =
    let
        table =
            List.map (\s -> div [] s)
                (List.map (printTableHand) round.table)

        players =
            List.map (showPlayer round.actualPlayer)
                (List.indexedMap (,) round.players)
    in
        div []
            ([ h2 [] [ text ("Round") ]
             , h3 [] [ text ("Table") ]
             ]
                ++ table
                ++ [ h3 [] [ text ("Players") ] ]
                ++ players
            )


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
            [ text player.name ]
        )
            :: printCards player.hand player.selection



-----------------------------------------------------------------------------
-- CSS STYLES
-----------------------------------------------------------------------------


cssStyle : Game -> String
cssStyle game =
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
"""
