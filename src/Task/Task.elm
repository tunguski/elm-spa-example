module Task.Task exposing (..)

import Html exposing (..)
import Html.App as App 
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


import Component exposing (..)
import Task.Add exposing (..)
import Task.Search exposing (..)


component : Component (Model msg) msg Msg
component = Component.simpleCp model update view


-- MODEL


type alias Model msg =
  { place : Pages
  , addComponent : Component Task.Add.Model msg Task.Add.Msg
  , searchComponent : Component Task.Search.Model msg Task.Search.Msg
  }


model : Model msg
model =
  Model
    Search
    Task.Add.component
    Task.Search.component


type Pages 
  = Search
  | Add


-- UPDATE


type Msg
  = SearchMsg Task.Search.Msg
  | AddMsg Task.Add.Msg


update : ComponentUpdate (Model msg) msg Msg 
update ctx action model =
  case action of
    AddMsg msg ->
      updateModel (mapContext ctx AddMsg) .addComponent 
        (\v m -> { m | addComponent = v }) model msg
    SearchMsg msg ->
      updateModel (mapContext ctx SearchMsg) .searchComponent 
        (\v m -> { m | searchComponent = v }) model msg


-- VIEW


view : ComponentView (Model msg) msg Msg 
view ctx model =
  case model.place of
    Add ->
      generateView (mapContext ctx AddMsg) model.addComponent
    Search ->
      generateView (mapContext ctx SearchMsg) model.searchComponent


