module Update exposing (update)

import Browser.Navigation as Navigation
import ClientApi exposing (..)
import Component exposing (..)
import Config exposing (..)
import LoginScreen
import Model exposing (..)
import ModelOps exposing (..)
import Msg exposing (..)
import Rest exposing (..)
import SessionModel exposing (..)
import TableView
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        None ->
            ( model
            , Cmd.none
            )

        GetSession result ->
            case Debug.log "getSession" result of
                Ok session ->
                    ( { model
                        | session = Just session
                        , tableComponent = TableView.component session.username model.tableComponent.model.name
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | session = Nothing }
                    , Cmd.none
                    )

        InitialWindowSize ->
            ( model
            , Cmd.none
            )

        ToggleHamburgerMenu ->
            let
                menu =
                    model.menu
            in
            ( { model | menu = { menu | expanded = not menu.expanded } }
            , Cmd.none
            )

        PlayAsGuest name ->
            ( model
            , get ("guest?name=" ++ name) sessions
                |> Task.attempt GetSession
            )

        Resize size ->
            let
                config =
                    model.config
            in
            ( { model | config = { config | windowSize = size } }
            , Cmd.none
            )

        ToggleSideMenu shorten ->
            let
                config =
                    model.config
            in
            ( { model | config = { config | smallSidebar = shorten } }
            , Cmd.none
            )

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
            ( newModel
            , Navigation.pushUrl model.key (toUrl newModel)
            )

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

        UrlRequest urlRequest ->
            processUrlRequest urlRequest model

        UrlChange url ->
            processUrlChange url model

        UrlUpdate location ->
            changeLocation location model
