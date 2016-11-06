module TableView exposing (..)

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
  }


model : Model
model =
  Model "" Nothing Nothing


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


init ctx =
  getTables baseUrl DoInit
    |> Cmd.map ctx.mapMsg


-- UPDATE


type Msg
    = TableName String
    | CreateNewTable
    | OpenTable String
    | DoInit (Result Http.Error (List Table))


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
    DoInit result ->
      case result of
        Ok tables ->
          { model | openTables = Just tables } ! []
        _ ->
          { model | openTables = Nothing } ! []


-- VIEW


view : ComponentView Model msg Msg 
view ctx model =
  Page "Table" <|
    fullRow
      [ h1 [] [ text "Table!" ]
      ]

