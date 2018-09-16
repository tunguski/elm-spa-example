module Layout exposing (componentSubsMap, generateMenu, generateMenuGroup, generatePageContent, mapMenuToHtml, menuClass, menuElement, onlyForLogged, printRow, topMenu, view)

import Browser exposing (Document)
import Browser.Events exposing (onResize)
import Component exposing (..)
import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List
import Model exposing (..)
import Msg exposing (..)



-- VIEW


topMenu : Model -> Html Msg
topMenu model =
    div []
        [ nav [ class "navbar navbar-default navbar-static-top" ]
            [ div [ class "navbar-header" ]
                (onlyForLogged model
                    [ button
                        [ class "navbar-toggle"
                        , onClick ToggleHamburgerMenu
                        ]
                        [ span [ class "sr-only" ] [ text "Toggle navigation" ]
                        , span [ class "icon-bar" ] []
                        , span [ class "icon-bar" ] []
                        , span [ class "icon-bar" ] []
                        ]
                    ]
                    ++ [ a [ class "navbar-brand" ] [ text "Elm Application" ]
                       , a [ class "navbar-brand nav navbar-top-links pull-right visible-xs" ] <|
                            onlyForLogged model [ i [ class "fa fa-power-off fa-fw" ] [] ]
                       ]
                )
            , a [ class "navbar-brand nav navbar-top-links navbar-right hidden-xs" ] <|
                onlyForLogged model [ i [ class "fa fa-power-off fa-fw" ] [] ]
            , div [ class "navbar-default sidebar" ]
                [ div [ class <| menuClass model ] <|
                    onlyForLogged model [ generateMenu model ]
                ]
            ]
        ]


onlyForLogged : Model -> List (Html Msg) -> List (Html Msg)
onlyForLogged model html =
    case model.session of
        Nothing ->
            []

        _ ->
            html


menuClass model =
    "sidebar-nav navbar-collapse "
        ++ (case model.menu.expanded of
                False ->
                    "collapsed"

                _ ->
                    ""
           )


generateMenu : Model -> Html Msg
generateMenu model =
    let
        allElements =
            List.append (List.concatMap generateMenuGroup model.menu.def)
                [ li
                    [ class "text-center menu-collapse hidden-xs side-menu-toggler"
                    , onClick (ToggleSideMenu (not model.config.smallSidebar))
                    ]
                    [ a []
                        [ text
                            (if model.config.smallSidebar then
                                ">"

                             else
                                "<"
                            )
                        ]
                    ]
                ]
    in
    ul [ class "nav in", id "side-menu" ] allElements


menuElement : Bool -> MenuElement -> Html Msg
menuElement isLast element =
    li
        [ class
            (if isLast then
                "last-in-group"

             else
                ""
            )
        ]
        [ a [ onClick <| ChangeView element.menuEntry ]
            [ i [ class ("fa fa-" ++ element.icon) ] []
            , span [] [ text element.title ]
            ]
        ]


mapMenuToHtml : List MenuElement -> List (Html Msg)
mapMenuToHtml elements =
    case elements of
        h :: [] ->
            [ menuElement True h ]

        h :: t ->
            menuElement False h :: mapMenuToHtml t

        [] ->
            []


generateMenuGroup : MenuGroup -> List (Html Msg)
generateMenuGroup group =
    let
        beginning =
            if group.title == "" then
                []

            else
                [ li [ class "menu-header" ] [ b [] [ text group.title ] ] ]

        tail =
            mapMenuToHtml group.elements
    in
    List.append beginning tail


generatePageContent : Model -> Page Msg
generatePageContent model =
    case model.session of
        Just session ->
            case model.place of
                ME_Dashboard ->
                    generateView (Context Dashboard) model.dashboardComponent

                ME_Table name ->
                    generateView (Context Table) model.tableComponent

                ME_Task page ->
                    generateView (Context Task) model.taskComponent

                ME_Member page ->
                    generateView (Context Member) model.memberComponent

                ME_Report page ->
                    generateView (Context Report) model.reportComponent

                ME_Tests ->
                    generateView (Context Tests) model.testsComponent

        Nothing ->
            generateView (Context Login) model.loginComponent


componentSubsMap : Model -> Sub Msg
componentSubsMap model =
    case model.session of
        Just session ->
            case model.place of
                ME_Dashboard ->
                    componentSubs (Context Dashboard) model.dashboardComponent

                ME_Table name ->
                    componentSubs (Context Table) model.tableComponent

                ME_Task page ->
                    componentSubs (Context Task) model.taskComponent

                ME_Member page ->
                    componentSubs (Context Member) model.memberComponent

                ME_Report page ->
                    componentSubs (Context Report) model.reportComponent

                ME_Tests ->
                    componentSubs (Context Tests) model.testsComponent

        Nothing ->
            componentSubs (Context Login) model.loginComponent


view : Model -> Document Msg
view model =
    { title = "Tichu Guru"
    , body = [ viewContent model ]
    }


viewContent : Model -> Html Msg
viewContent model =
    let
        page =
            generatePageContent model
    in
    div
        [ class <|
            "site-wrapper "
                ++ (if model.config.smallSidebar then
                        "sidebar-small"

                    else
                        ""
                            ++ (if model.session == Nothing then
                                    "anonymous"

                                else
                                    ""
                               )
                   )
        ]
        [ node "style" [ type_ "text/css" ] [ text (coverCss model.config) ]
        , page.content
        ]


printRow : List (Html Msg) -> Html Msg
printRow content =
    div [ class "row" ] [ div [ class "col-md-12" ] content ]
