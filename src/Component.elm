module Component exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- COMPONENT MODEL
-- cMsg - component's Msg


type alias Context msg cMsg =
    { mapMsg : cMsg -> msg
    }



-- cMsg - component's Msg


type alias Component model msg cMsg =
    { model : model
    , update : Context msg cMsg -> cMsg -> model -> ( model, Cmd msg )
    , view : Context msg cMsg -> model -> Page msg
    , initCmd : Maybe (Context msg cMsg -> Cmd msg)
    , subs : Maybe (Context msg cMsg -> model -> Sub msg)
    }


simpleCp model update view =
    Component model update view Nothing Nothing


type alias ComponentUpdate model msg cMsg =
    Context msg cMsg -> cMsg -> model -> ( model, Cmd msg )


type alias ComponentView model msg cMsg =
    Context msg cMsg -> model -> Page msg


type alias Page msg =
    { title : String
    , content : Html msg
    }


mapContext : Context msg cMsg -> (ccMsg -> cMsg) -> Context msg ccMsg
mapContext ctx map =
    { ctx | mapMsg = map >> ctx.mapMsg }


generateView : Context msg cMsg -> Component model msg cMsg -> Page msg
generateView ctx cp =
    cp.view ctx cp.model


componentSubs : Context msg cMsg -> Component model msg cMsg -> Sub msg
componentSubs ctx component =
    case component.subs of
        Just fn ->
            fn ctx component.model

        _ ->
            Sub.none



{-  -}
--updateModel : Context msg cMsg -> (model -> Component cModel cMsg msg) -> (Component cModel cMsg msg -> model -> model) -> model -> cMsg -> (model, Cmd msg)


updateModel ctx getter setter model msg =
    let
        cp =
            Debug.log "updateModel" <| getter model

        ( updatedModel, cmd ) =
            cp.update ctx msg cp.model
    in
        ( setter { cp | model = updatedModel } model, cmd )



{-  -}
--setPlaceInnerComponent : (Model -> Component cModel cMsg) -> (Component cModel cMsg -> Model -> Model) -> Model -> MenuEntry -> page -> (Model, Cmd Msg)


setPlaceInnerComponent getter setter model place page =
    let
        cp =
            getter model

        m =
            cp.model
    in
        (setter { cp | model = { m | place = page } }
            { model | place = place }
        )
            ! []



--setPlace : (Model -> Component cModel cMsg) -> Model -> MenuEntry -> (Model, Cmd Msg)


setPlace ctx getter model place =
    let
        cp =
            getter model
    in
        { model | place = place }
            ! (case cp.initCmd of
                Just c ->
                    [ c ctx ]

                Nothing ->
                    []
              )


row : List (Html msg) -> Html msg
row internal =
    div [ class "row" ] internal


fullRow : List (Html msg) -> Html msg
fullRow internal =
    row [ div [ class "col-md-12" ] internal ]


singleCellRow : Int -> List (Html msg) -> Html msg
singleCellRow width internal =
    row [ div [ class <| "col-md-" ++ (toString width) ] internal ]


multiCellRow : List ( Int, List (Html msg) ) -> Html msg
multiCellRow cols =
    row <|
        List.map
            (\( width, internal ) ->
                div [ class <| "col-md-" ++ (toString width) ] internal
            )
            cols


headerRow : String -> Html msg
headerRow header =
    div [ class "row header" ] [ h2 [] [ text header ] ]
