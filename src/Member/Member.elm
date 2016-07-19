module Member.Member exposing (..)

import Html exposing (..)
import Html.App as App 
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


import Component exposing (..)
import Member.Add exposing (..)
import Member.Search exposing (..)


component : Component (Model msg) msg Msg
component = Component model update view


-- MODEL


type alias Model msg =
  { place : Pages
  , addComponent : Component Member.Add.Model msg Member.Add.Msg
  , searchComponent : Component Member.Search.Model msg Member.Search.Msg
  }


model : Model msg
model =
  Model
    Search
    Member.Add.component
    Member.Search.component


type Pages 
  = Search
  | Add


-- UPDATE


type Msg
  = SearchMsg Member.Search.Msg
  | AddMsg Member.Add.Msg


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


