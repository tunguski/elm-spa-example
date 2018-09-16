module Msg exposing (MenuEntry(..), Msg(..))

import Browser
import Component exposing (Component)
import Config
import Dashboard
import Http
import LoginScreen
import Member.Member as Member
import Report.Report as Report
import SessionModel exposing (Session)
import TableView
import Task.Task as Task
import Tests
import Url exposing (Url)


type Msg
    = None
    | UrlUpdate (Maybe MenuEntry)
    | UrlRequest Browser.UrlRequest
    | UrlChange Url
    | InitialWindowSize
    | ToggleHamburgerMenu
    | PlayAsGuest String
    | GetSession (Result Http.Error Session)
    | Resize Int Int
    | ToggleSideMenu Bool
    | ChangeView MenuEntry
    | Login LoginScreen.Msg
    | Dashboard Dashboard.Msg
    | Task Task.Msg
    | Member Member.Msg
    | Report Report.Msg
    | Table TableView.Msg
    | Tests Tests.Msg


type MenuEntry
    = ME_Dashboard
    | ME_Table String
    | ME_Task Task.Pages
    | ME_Member Member.Pages
    | ME_Report Report.Pages
    | ME_Tests
