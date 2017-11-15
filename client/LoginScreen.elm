module LoginScreen exposing (..)

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Config exposing (..)
import Component exposing (..)
import SessionModel exposing (..)


component : (String -> msg) -> Component Model msg Msg
component msg =
    Component.simpleCp model update (view msg)



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


update : ComponentUpdate Model msg Msg
update ctx action model =
    case action of
        Name name ->
            { model | name = name } ! []

        Password password ->
            { model | password = password } ! []

        PasswordAgain password ->
            { model | passwordAgain = password } ! []



-- VIEW


view : (String -> msg) -> ComponentView Model msg Msg
view playAsGuest ctx model =
    Page "Login" <|
    div [ class "site-wrapper" ]
        [ div [ class "site-wrapper-inner" ]
            [ div [ class "cover-container" ]
                [ div [ class "masthead clearfix" ]
                    [ div [ class "inner" ]
                        [ h3 [ class "masthead-brand" ]
                            [ text "Tichu Guru "
                            , small [] [ text "(beta)" ]
                            ]
                        ]
                    ]
                , div [ class "inner cover" ]
                    [ h1 [ class "cover-heading" ] [ text "House of Tichu" ]
                    , p [ class "lead" ]
                        [ text "Tichu Guru is a site created with single best card game in mind." ]
                    , p [ class "lead" ]
                        [ input
                            [ class "form-control w-50 login-username"
                            , type_ "text"
                            , placeholder "Please enter username"
                            , onInput <| Name >> ctx.mapMsg
                            ] [] ]
                    , p [ class "lead" ]
                        [ button
                            ((if String.length model.name > 4 then
                                onClick (playAsGuest model.name)
                             else
                                disabled True
                            )
                            ::
                            [ class "btn btn-lg btn-secondary"
                            , type_  "button"
                            ])
                            [ text "Play!" ]
                        ]
                    ]
                , div [ class "mastfoot" ]
                    [ div [ class "inner" ]
                        [ p [] [ text "Tichu Guru Team 2017" ]
                        ]
                    ]
                --, div [ class "blur" ] []
                ]
            ]
        ]


