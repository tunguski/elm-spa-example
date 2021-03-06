module Update exposing (..)

import Navigation
import Task


import ClientApi exposing (..)
import Config exposing (..)
import Component exposing (..)
import Msg exposing (..)
import Model exposing (..)
import ModelOps exposing (..)
import LoginScreen
import Rest exposing (..)
import SessionModel exposing (..)
import TableView


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        None ->
            model ! []

        GetSession result ->
            case (Debug.log "getSession" result) of
                Ok session ->
                    { model
                    | session = Just session
                    , tableComponent = TableView.component session.username model.tableComponent.model.name
                    } ! []

                Err error ->
                    { model | session = Nothing } ! []

        InitialWindowSize ->
            model ! []

        ToggleHamburgerMenu ->
            let
                menu =
                    model.menu
            in
                { model | menu = { menu | expanded = not menu.expanded } } ! []

        PlayAsGuest name ->
            model ! [ get ("guest?name=" ++ name) sessions
                      |> Task.attempt GetSession ]

        Resize size ->
            let
                config =
                    model.config
            in
                { model | config = { config | windowSize = size } } ! []

        ToggleSideMenu shorten ->
            let
                config =
                    model.config
            in
                { model | config = { config | smallSidebar = shorten } } ! []

        ChangeView entry ->
            let
                menu =
                    model.menu

                newModel =
                    { model
                        | place = entry
                        , menu = { menu | expanded = False }
                    }
            in
                newModel ! [ Navigation.newUrl (toUrl newModel) ]

        Dashboard msg ->
            Component.updateModel (Context Dashboard)
                .dashboardComponent
                setDashboardComponent
                model
                msg

        Table msg ->
            Component.updateModel (Context Table)
                .tableComponent
                setTableComponent
                model
                msg

        Login msg ->
            Component.updateModel (Context Login)
                .loginComponent
                setLoginComponent
                model
                msg

        Task msg ->
            Component.updateModel (Context Task)
                .taskComponent
                setTaskComponent
                model
                msg

        Member msg ->
            Component.updateModel (Context Member)
                .memberComponent
                setMemberComponent
                model
                msg

        Report msg ->
            Component.updateModel (Context Report)
                .reportComponent
                setReportComponent
                model
                msg

        Tests msg ->
            Component.updateModel (Context Tests)
                .testsComponent
                setTestsComponent
                model
                msg

        UrlUpdate location ->
            changeLocation location model


