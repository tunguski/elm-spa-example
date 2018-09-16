module Model exposing (CssConfig, Menu, MenuElement, MenuGroup, Model, setDashboardComponent, setLoginComponent, setMemberComponent, setReportComponent, setTableComponent, setTaskComponent, setTestsComponent)

import Browser.Navigation as Navigation exposing (Key)
import Component exposing (Component)
import Config
import Dashboard
import Http
import LoginScreen
import Member.Member as Member
import Msg exposing (..)
import Report.Report as Report
import SessionModel exposing (Session)
import TableView
import Task.Task as Task
import Tests



-- MODEL


type alias Model =
    { key : Key
    , baseUrl : String
    , place : MenuEntry
    , session : Maybe Session
    , menu : Menu
    , config : CssConfig
    , dashboardComponent : Component Dashboard.Model Msg Dashboard.Msg
    , tableComponent : Component TableView.Model Msg TableView.Msg
    , loginComponent : Component LoginScreen.Model Msg LoginScreen.Msg
    , taskComponent : Component (Task.Model Msg) Msg Task.Msg
    , memberComponent : Component (Member.Model Msg) Msg Member.Msg
    , reportComponent : Component (Report.Model Msg) Msg Report.Msg
    , testsComponent : Component Tests.Model Msg Tests.Msg
    }


setDashboardComponent component model =
    { model | dashboardComponent = component }


setTableComponent component model =
    { model | tableComponent = component }


setLoginComponent component model =
    { model | loginComponent = component }


setTaskComponent component model =
    { model | taskComponent = component }


setMemberComponent component model =
    { model | memberComponent = component }


setReportComponent component model =
    { model | reportComponent = component }


setTestsComponent component model =
    { model | testsComponent = component }


type alias CssConfig =
    { windowSize : ( Int, Int )
    , smallSidebar : Bool
    }


type alias MenuElement =
    { title : String
    , menuEntry : MenuEntry
    , icon : String
    }


type alias MenuGroup =
    { title : String
    , elements : List MenuElement
    }


type alias Menu =
    { small : Bool
    , expanded : Bool
    , def : List MenuGroup
    }
