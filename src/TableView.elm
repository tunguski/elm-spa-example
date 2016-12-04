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
component = Component model update view (Nothing) Nothing


-- MODEL


type alias Model =
  { table : Maybe Table
  }


model : Model
model =
  Model Nothing


getTable : String -> (Result Http.Error Table -> msg) -> Cmd msg
getTable baseUrl msg =
  Http.get tableDecoder
  (baseUrl ++ "tables")
    |> Task.perform Err Ok
    |> Cmd.map msg


init ctx =
  getTable baseUrl (\s -> LoadChanges)
    |> Cmd.map ctx.mapMsg


-- UPDATE


type Msg
  = LoadChanges


update : ComponentUpdate Model msg Msg
update ctx action model =
  case action of
    LoadChanges ->
      model ![]


-- VIEW


view : ComponentView Model msg Msg
view ctx model =
  Page "Table" <|
    multiCellRow
      [ (2, [ div [ class "table-chat" ]
          [ div [ class "chat-header" ] [ text "Chat" ]
          , div [] [ text "fsdds" ]
          , div [] [ text "asdf" ]
          ] ])
      , (8, [ div [ class "table-main" ] [ text "main" ] ])
      , (2, [ div [ class "table-options" ]
          [div [ class "table-options-header" ] [ text "Game" ]
          ] ])
      ]

