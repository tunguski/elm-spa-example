module LoginScreen exposing (..)


import Debug
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


import Config exposing (..)
import Component exposing (..)
import ClientSession exposing (..)


component : Component Model msg Msg
component = Component model update view


-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


model : Model
model =
  Model "" "" ""


-- MSG 


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | PlayAsGuest
    | GetSession (Result Http.Error Session)


update : ComponentUpdate Model msg Msg 
update ctx action model =
  case action of
    Name name ->
      { model | name = name } ! []

    Password password ->
      { model | password = password } ! []

    PasswordAgain password ->
      { model | passwordAgain = password } ! []

    PlayAsGuest ->
      (Debug.log "Play as Guest" model) 
      ! [ createGuestSession baseUrl <| GetSession >> ctx.mapMsg ]

    GetSession result ->
      Debug.crash "Can never be here" <| model ! []


-- VIEW


view : ComponentView Model msg Msg 
view ctx model =
  Page "Login" <|
    Html.form [ class "form" ] [
    div [ class "col-md-offset-4 col-md-4" ]
      [ legend [] [ text "Please login" ]
      , Html.form 
        [ class "form-horizontal" ]
        [ div [ class "form-group" ] 
          [ label 
            [ class "col-sm-3 control-label" ] 
            [ text "Login" ]
          , div [ class "col-sm-9" ] 
            [ input 
              [ type' "text" 
              , class "form-control" 
              , placeholder "Login"
              , onInput <| Name >> ctx.mapMsg
              ] []
            ]
          ]
        , div [ class "form-group" ] 
          [ label 
            [ class "col-sm-3 control-label" ]
            [ text "Password" ]
          , div [ class "col-sm-9" ] 
            [ input 
              [ type' "text" 
              , class "form-control" 
              , placeholder "Password"
              , onInput <| Name >> ctx.mapMsg
              ] []
            ]
          ]
        , div [ class "form-group" ] 
          [ div [ class "col-sm-offset-3 col-sm-9" ] 
            [ button
              [ type' "button"
              , class "btn btn-primary"
              --, onClick Login
              ] [ text "Login" ]
            , button
              [ type' "button"
              , class "btn btn-default"
              , onClick <| ctx.mapMsg PlayAsGuest 
              ] [ text "Play as Guest" ]
            ]
          ]
        ]
      ]
      ]


