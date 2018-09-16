module LoginScreen exposing (Model, Msg(..), component, emptyModel, update, view)

import Component exposing (..)
import Config exposing (..)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import SessionModel exposing (..)


component : (String -> msg) -> Component Model msg Msg
component msg =
    Component.simpleCp emptyModel update (view msg)



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


emptyModel : Model
emptyModel =
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
            ( { model | name = name }
            , Cmd.none
            )

        Password password ->
            ( { model | password = password }
            , Cmd.none
            )

        PasswordAgain password ->
            ( { model | passwordAgain = password }
            , Cmd.none
            )



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
                                ]
                                []
                            ]
                        , p [ class "lead" ]
                            [ button
                                ((if String.length model.name > 4 then
                                    onClick (playAsGuest model.name)

                                  else
                                    disabled True
                                 )
                                    :: [ class "btn btn-lg btn-secondary"
                                       , type_ "button"
                                       ]
                                )
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
