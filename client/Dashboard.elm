module Dashboard exposing (Model, Msg(..), Player, component, emptyModel, gameTypeRadio, init, subs, update, view)

import BaseModel exposing (..)
import Browser.Navigation as Navigation exposing (Key)
import ClientApi exposing (..)
import Component exposing (..)
import Config exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Json exposing (..)
import Rest exposing (..)
import Result exposing (toMaybe)
import Task
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import Time exposing (..)


component : Key -> Component Model msg Msg
component key =
    Component (emptyModel key) update view (Just init) (Just subs)



-- MODEL


type alias Player =
    { name : String
    }


type alias Model =
    { key : Key
    , newTableName : String
    , createNewTable : Bool
    , openTables : Maybe (List AwaitingTable)
    , players : Maybe (List Player)
    , lastFinishedTableUpdate : Maybe Posix
    , gameConfig : GameConfig
    }


emptyModel : Key -> Model
emptyModel key =
    Model key "" False Nothing Nothing Nothing (GameConfig Humans)


init : Context msg Msg -> Cmd msg
init ctx =
    Task.map2 (\a b -> ( a, b ))
        now
        (findAll awaitingTables)
        |> Task.attempt (UpdateTables >> ctx.mapMsg)


subs : Context msg Msg -> model -> Sub msg
subs ctx model =
    every second CheckUpdate
        |> Sub.map ctx.mapMsg


second =
    1000.0



-- UPDATE


type Msg
    = TableName String
    | CreateNewTable
    | SendCreateNewTable
    | OpenTable String
    | UpdateTables (Result Http.Error ( Posix, List AwaitingTable ))
    | CheckUpdate Posix
    | SwitchTo GameType


update : ComponentUpdate Model msg Msg
update ctx action model =
    case action of
        TableName name ->
            ( { model | newTableName = name }
            , Cmd.none
            )

        SwitchTo gameType ->
            let
                gameConfig =
                    model.gameConfig
            in
            ( { model | gameConfig = { gameConfig | gameType = gameType } }
            , Cmd.none
            )

        CreateNewTable ->
            ( { model | createNewTable = True }
            , Cmd.none
            )

        SendCreateNewTable ->
            ( model
            , awaitingTables
                |> withBody (encodeGameConfig model.gameConfig)
                |> postCommand model.newTableName
                |> Task.attempt (\r -> ctx.mapMsg <| OpenTable model.newTableName)
            )

        OpenTable name ->
            ( model
            , Navigation.pushUrl model.key ("#/Table/" ++ name)
            )

        UpdateTables result ->
            case result of
                Ok ( time, tables ) ->
                    ( { model
                        | openTables = Just tables
                        , lastFinishedTableUpdate = Just time
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CheckUpdate time ->
            case model.lastFinishedTableUpdate of
                Just last ->
                    if Time.posixToMillis last + (3 * Basics.round second) < Time.posixToMillis time then
                        ( { model
                            | lastFinishedTableUpdate = Nothing
                          }
                        , init ctx
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                _ ->
                    ( model
                    , Cmd.none
                    )



-- VIEW


gameTypeRadio ctx isChecked description gameType =
    div [ class "form-group" ]
        [ label [ class "radio-inline" ]
            [ input
                ([ type_ "radio"
                 , name "game-type"
                 , onClick <| ctx.mapMsg <| SwitchTo gameType
                 ]
                    ++ (case isChecked of
                            True ->
                                [ checked True ]

                            False ->
                                []
                       )
                )
                []
            , text description
            ]
        ]


view : ComponentView Model msg Msg
view ctx model =
    Page "Dashboard" <|
        div [ class "container-fluid" ]
            [ nav [ class "navbar" ]
                [ div [ class "masthead clearfix" ]
                    [ div [ class "inner" ]
                        [ h3 [ class "masthead-brand" ]
                            [ text "Tichu Guru "
                            , small [] [ text "(beta)" ]
                            ]
                        ]
                    ]
                ]
            , h3 [ class "thin" ] [ text "Open Tables" ]
            , div [ class "row" ] <|
                div
                    [ class
                        (if model.createNewTable then
                            "col-12"

                         else
                            "col-md-3 col-xl-2 col-sm-6"
                        )
                    ]
                    [ div
                        [ class "game-card"
                        , onClick <| ctx.mapMsg CreateNewTable
                        ]
                        (if model.createNewTable then
                            --[ text "Describe new table"
                            [ div [ class "form-group" ]
                                [ label [] [ text "Table Name" ]
                                , input
                                    [ class "form-control table-name"
                                    , placeholder "Unique name"
                                    , onInput <| TableName >> ctx.mapMsg
                                    ]
                                    []
                                ]
                            , gameTypeRadio ctx (model.gameConfig.gameType == Humans) "Humans only" Humans
                            , gameTypeRadio ctx False "Humans vs. bots" HumansVsBots
                            , gameTypeRadio ctx False "Play with bots" Bots
                            , button
                                ([ class "btn btn-primary"
                                 , type_ "button"
                                 ]
                                    ++ (case String.length model.newTableName >= 3 of
                                            True ->
                                                [ onClick <| ctx.mapMsg SendCreateNewTable ]

                                            False ->
                                                [ disabled True ]
                                       )
                                )
                                [ text "Create New Table" ]
                            ]

                         else
                            [ text "New Table"
                            , div [ class "add-table-button" ]
                                [ text "+" ]
                            ]
                        )
                    ]
                    :: (List.map
                            (\name ->
                                div [ class "col-md-3 col-xl-2 col-sm-6" ]
                                    [ div [ class "game-card" ]
                                        [ text name
                                        ]
                                    ]
                            )
                        <|
                            List.map (\i -> Debug.toString i) <|
                                List.range 0 30
                       )
            , footer [ class "default-footer" ]
                [ div [ class "" ]
                    [ p [] [ text "Tichu Guru Team 2017" ]
                    ]
                ]
            ]



--view : ComponentView Model msg Msg
--view ctx model =
--    Page "Dashboard" <|
--        fullRow
--            [ Html.form [ class "form-inline" ]
--                [ div [ class "form-group" ]
--                    [ label [] [ text "Table Name" ]
--                    , input
--                        [ class "form-control"
--                        , placeholder "Unique name"
--                        , onInput <| TableName >> ctx.mapMsg
--                        ]
--                        []
--                    ]
--                , gameTypeRadio ctx (model.gameConfig.gameType == Humans) "Humans only" Humans
--                , gameTypeRadio ctx False "Humans vs. bots" HumansVsBots
--                , gameTypeRadio ctx False "Play with bots" Bots
--                , button
--                    ([ class "btn btn-primary"
--                    , type_ "button"
--                    ]
--                    ++
--                      case String.length model.newTableName >= 3 of
--                        True ->
--                            [ onClick <| ctx.mapMsg SendCreateNewTable ]
--                        False ->
--                            [ disabled True ]
--                    )
--                    [ text "Create New Table" ]
--                ]
--            , h3 [] [ text "Open Tables" ]
--            , table [ class "table table-striped x" ]
--                [ thead []
--                    [ tr []
--                        [ th [] [ text "Name" ]
--                        , th [] []
--                        , th [] [ text "Player" ]
--                        ]
--                    ]
--                , tbody []
--                    (case model.openTables of
--                        Just tables ->
--                            tables|> List.map (\table ->
--                                tr []
--                                    [ td [] [ a [ href ("#/Table/" ++ table.name) ] [ text table.name ] ]
--                                    , td [] [ text (toString (List.length table.users) ++ "/4") ]
--                                    , td []
--                                        (List.map (.name >> text) table.users)
--                                    ]
--                            )
--
--                        Nothing ->
--                            []
--                    )
--                ]
--            , h3 [] [ text "Players" ]
--            , table [ class "table table-striped x" ]
--                [ thead []
--                    [ tr []
--                        [ th [] [ text "Name" ]
--                        , th [] [ text "Ranking" ]
--                        , th [] [ text "Stats" ]
--                        ]
--                    ]
--                , tbody []
--                    [ tr []
--                        [ td [] [ text "Name" ]
--                        , td [] [ text "Player" ]
--                        , td [] [ text "Stats" ]
--                        ]
--                    , tr []
--                        [ td [] [ text "Name" ]
--                        , td [] [ text "Player" ]
--                        , td [] [ text "Stats" ]
--                        ]
--                    ]
--                ]
--            ]
