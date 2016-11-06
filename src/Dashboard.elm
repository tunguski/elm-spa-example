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


import Config exposing (..)
import Component exposing (..)


component : Component Model msg Msg
component = Component model update view (Just init) Nothing


-- MODEL


type alias Table =
  { name : String
  }


type alias Player =
  { name : String
  }


type alias Model =
  { newTableName : String
  , openTables : Maybe (List Table)
  , players : Maybe (List Player)
  }


model : Model
model =
  Model "" Nothing Nothing


getTables : String -> (Result Http.Error String -> msg) -> Cmd msg
getTables baseUrl msg =
  Http.get Json.string
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


init ctx =
  getTables baseUrl DoInit
    |> Cmd.map ctx.mapMsg


-- UPDATE


type Msg
    = TableName String
    | CreateNewTable
    | OpenTable String
    | DoInit (Result Http.Error String)


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
    DoInit string ->
      model ! []


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
              , th [] [ text "Player" ]
              ]
            ]
        , tbody []
            [ tr []
              [ td [] [ a [ href "" ] [ text "Name" ] ]
              , td [] [ text "Player" ]
              ]
            , tr []
              [ td [] [ a [ href "" ] [ text "Name" ] ]
              , td [] [ text "Player" ]
              ]
            ]
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


