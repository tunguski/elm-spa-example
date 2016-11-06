module Model exposing (..)


import Window exposing (Size)
import Http


import Config
import Component exposing (Component)
import Task.Task as Task
import Member.Member as Member
import Report.Report as Report
import Dashboard
import TableView
import SessionModel exposing (Session)
import LoginScreen
import Msg exposing (..)


-- MODEL


type alias Model =
  { baseUrl : String
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


type alias CssConfig =
  { windowSize : Size 
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


