module Report.Report exposing (Model, Msg(..), Pages(..), component, emptyModel, update, view)

import Component exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Report.Add exposing (..)
import Report.Search exposing (..)


component : Component (Model msg) msg Msg
component =
    Component.simpleCp emptyModel update view



-- MODEL


type alias Model msg =
    { place : Pages
    , addComponent : Component Report.Add.Model msg Report.Add.Msg
    , searchComponent : Component Report.Search.Model msg Report.Search.Msg
    }


emptyModel : Model msg
emptyModel =
    Model
        Search
        Report.Add.component
        Report.Search.component


type Pages
    = Search
    | Add



-- UPDATE


type Msg
    = SearchMsg Report.Search.Msg
    | AddMsg Report.Add.Msg


update : ComponentUpdate (Model msg) msg Msg
update ctx action model =
    case action of
        AddMsg msg ->
            updateModel (mapContext ctx AddMsg)
                .addComponent
                (\v m -> { m | addComponent = v })
                model
                msg

        SearchMsg msg ->
            updateModel (mapContext ctx SearchMsg)
                .searchComponent
                (\v m -> { m | searchComponent = v })
                model
                msg



-- VIEW


view : ComponentView (Model msg) msg Msg
view ctx model =
    case model.place of
        Add ->
            generateView (mapContext ctx AddMsg) model.addComponent

        Search ->
            generateView (mapContext ctx SearchMsg) model.searchComponent
