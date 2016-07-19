module Model exposing (..)


import Window exposing (Size)
import Http


import Config
import Component exposing (Component)
import Task.Task as Task
import Member.Member as Member
import Report.Report as Report
import Dashboard
import ClientSession exposing (Session)
import LoginScreen


-- MODEL


type alias Model =
  { baseUrl : String
  , place : MenuEntry 
  , session : Maybe Session 
  , menu : Menu
  , config : CssConfig
  , dashboardComponent : Component Dashboard.Model Msg Dashboard.Msg
  , loginComponent : Component LoginScreen.Model Msg LoginScreen.Msg
  , taskComponent : Component (Task.Model Msg) Msg Task.Msg
  , memberComponent : Component (Member.Model Msg) Msg Member.Msg
  , reportComponent : Component (Report.Model Msg) Msg Report.Msg
  }


setDashboardComponent component model =
  { model | dashboardComponent = component }


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


type Msg
  = None
  | GetSession (Result Http.Error Session)
  | InitialWindowSize
  | ToggleHamburgerMenu 

  | Resize Size 
  | ToggleSideMenu Bool 
  | ChangeView MenuEntry

  | Login LoginScreen.Msg
  | Dashboard Dashboard.Msg 
  | Task Task.Msg
  | Member Member.Msg
  | Report Report.Msg


type MenuEntry
  = ME_Dashboard 
  | ME_Task Task.Pages
  | ME_Member Member.Pages
  | ME_Report Report.Pages


