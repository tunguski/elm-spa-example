module Dashboard exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Navigation 
import Json.Decode as Json exposing (..)
import Result exposing (toMaybe)
import Http
import Task
import Time exposing (now, Time)


import Config exposing (..)
import Component exposing (..)
import BaseModel exposing (..)
import TableModel exposing (..)


component : Component Model msg Msg
component = Component model update view (Just init) Nothing


-- MODEL


type alias Player =
  { name : String
  }


type alias Model =
  { newTableName : String
  , openTables : Maybe (List Table)
  , players : Maybe (List Player)
  , lastFinishedTableUpdate : Maybe Time
  }


model : Model
model =
  Model "" Nothing Nothing Nothing


getTables : String -> (Result Http.Error (List Table) -> msg) -> Cmd msg
getTables baseUrl msg =
  Http.get (listDecoder tableDecoder)
  (baseUrl ++ "tables")
    |> Task.perform Err Ok
    |> Cmd.map msg 


createTable : String -> String -> (Result Http.Error String -> msg) -> Cmd msg
createTable baseUrl tableName msg =
  Http.post Json.string
  (baseUrl ++ "tables")
  (Http.string tableName)
    |> Task.perform Err Ok
    |> Cmd.map msg 


init : Context msg Msg -> Cmd msg
init ctx =
  Task.map2
    (,)
    --(\a b -> ctx.mapMsg (UpdateTables a b))
    --(UpdateTables >> ctx.mapMsg)
    now
    (Http.get (listDecoder tableDecoder)
      (baseUrl ++ "tables"))
  |> Task.perform Err Ok
  |> Cmd.map UpdateTables
  |> Cmd.map ctx.mapMsg


-- UPDATE


type Msg
    = TableName String
    | CreateNewTable
    | OpenTable String
    | UpdateTables (Result Http.Error (Time ,List Table))


update : ComponentUpdate Model msg Msg 
update ctx action model =
  case action of
    TableName name ->
      { model | newTableName = name } ! []
    CreateNewTable ->
      model 
        ! [ createTable baseUrl model.newTableName 
              (\r -> ctx.mapMsg <| OpenTable model.newTableName) 
          ]
    OpenTable name ->
      model 
        ! [ Navigation.newUrl ("#/Table/" ++ name) ]
    UpdateTables result ->
      case result of
        Ok (time, tables) ->
          { model 
            | openTables = Just tables
            , lastFinishedTableUpdate = Just time
          } ! []
        _ ->
          { model | openTables = Nothing } ! []


-- VIEW


view : ComponentView Model msg Msg 
view ctx model =
  Page "Dashboard" <|
    fullRow
      [ Html.form [ class "form-inline" ]
          [ div [ class "form-group" ]
              [ label [] [ text "Table Name" ]
              , input
                  [ class "form-control" 
                  , placeholder "Unique name"
                  , onInput <| TableName >> ctx.mapMsg
                  ]
                  []
              ]
          , button 
              [ class "btn btn-primary"
              , onClick <| ctx.mapMsg CreateNewTable 
              , type' "button"
              ] 
              [ text "Create New Table" ]
          ]
      , h3 [] [ text "Open Tables" ]
      , table [ class "table table-striped x" ]
        [ thead []
            [ tr []
              [ th [] [ text "Name" ]
              , th [] []
              , th [] [ text "Player" ]
              ]
            ]
        , tbody [] 
            (case model.openTables of
              Just tables ->
                tables |> List.map (\table ->
                  tr []
                    [ td [] [ a [ href ("#/Table/" ++ table.name) ] [ text table.name ] ]
                    , td [] [ text ( toString (List.length table.players) ++ "/4" ) ]
                    , td [] (List.map text table.players)
                    ]
                )
              Nothing ->
                []
            )
        ] 
      , h3 [] [ text "Players" ]
      , table [ class "table table-striped x" ]
        [ thead []
            [ tr []
              [ th [] [ text "Name" ]
              , th [] [ text "Ranking" ]
              , th [] [ text "Stats" ]
              ]
            ]
        , tbody []
            [ tr []
              [ td [] [ text "Name" ]
              , td [] [ text "Player" ]
              , td [] [ text "Stats" ]
              ]
            , tr []
              [ td [] [ text "Name" ]
              , td [] [ text "Player" ]
              , td [] [ text "Stats" ]
              ]
            ]
        ] 
      ]


