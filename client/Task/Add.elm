module Task.Add exposing (Model, Msg(..), component, update, view)

import Component exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


component : Component Model msg Msg
component =
    Component.simpleCp emptyModel update view



-- MODEL


type alias Model =
    { name : String
    }


emptyModel : Model
emptyModel =
    Model ""



-- UPDATE


type Msg
    = Name String


update : ComponentUpdate Model msg Msg
update ctx action model =
    case action of
        Name name ->
            ( { model | name = name }
            , Cmd.none
            )



-- VIEW


view : ComponentView Model msg Msg
view ctx model =
    Page "Task Add" <|
        fullRow
            [ legend [] [ text "Add task" ]
            , Html.form [ class "form-horizontal" ]
                [ div [ class "form-group" ]
                    [ label [ class "col-sm-3 control-label" ] [ text "Title" ]
                    , div [ class "col-sm-9" ]
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , placeholder "Title"
                            , onInput <| Name >> ctx.mapMsg
                            ]
                            []
                        ]
                    ]
                , div [ class "form-group" ]
                    [ label [ class "col-sm-3 control-label" ] [ text "Priority" ]
                    , div [ class "col-sm-9" ]
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , placeholder "Priority"
                            , onInput <| Name >> ctx.mapMsg
                            ]
                            []
                        ]
                    ]
                , div [ class "form-group" ]
                    [ div [ class "col-sm-offset-3 col-sm-9" ]
                        [ button
                            [ type_ "button"
                            , class "btn btn-primary"

                            --              , onClick Name
                            ]
                            [ text "Add" ]
                        ]
                    ]
                ]
            ]
