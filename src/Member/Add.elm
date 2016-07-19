module Member.Add exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


import Component exposing (..)


component : Component Model msg Msg
component = Component model update view


-- MODEL


type alias Model =
  { name : String
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
    Name name ->
      { model | name = name } ! []


-- VIEW


view : ComponentView Model msg Msg 
view ctx model =
  Page "Member Add" <|
    fullRow
      [ legend [] [ text "Add member" ]
      , Html.form [ class "form-horizontal" ]
        [ div [ class "form-group" ] 
          [ label [ class "col-sm-3 control-label" ] [ text "Name" ]
          , div [ class "col-sm-9" ] 
            [ input 
              [ type' "text" 
              , class "form-control" 
              , placeholder "Name"
              , onInput <| Name >> ctx.mapMsg
              ] []
            ]
          ]
        , div [ class "form-group" ] 
          [ label [ class "col-sm-3 control-label" ] [ text "Groups" ]
          , div [ class "col-sm-9" ] 
            [ input 
              [ type' "text" 
              , class "form-control" 
              , placeholder "Groups"
              , onInput <| Name >> ctx.mapMsg
              ] []
            ]
          ]
        , div [ class "form-group" ] 
          [ div [ class "col-sm-offset-3 col-sm-9" ] 
            [ button 
              [ type' "button" 
              , class "btn btn-primary" 
--              , onClick Name 
              ] [ text "Add" ]
            ]
          ]
        ]
      ]


