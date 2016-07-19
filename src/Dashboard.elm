module Dashboard exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


import Component exposing (..)


component : Component Model msg Msg
component = Component model update view


-- MODEL


type alias Model =
  { data : String
  }


model : Model
model =
  Model ""


-- UPDATE


type Msg
    = Name String


update : ComponentUpdate Model msg Msg 
update ctx action model =
  case action of
    Name data ->
      { model | data = data } ! []


-- VIEW


view : ComponentView Model msg Msg 
view ctx model =
  Page "Dashboard" <|
    fullRow
      [ input 
        [ type' "text"
        , placeholder "Name"
        , onInput <| Name >> ctx.mapMsg
        ] []
      ]


