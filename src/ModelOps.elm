module ModelOps exposing (..)


import Navigation
import Task exposing (perform)
import Window exposing (Size)
import Regex exposing (..)
import String exposing (dropLeft)


import Config exposing (..)
import Model exposing (..)
import Component exposing (..)
import Task.Task as Task
import Member.Member as Member
import Report.Report as Report
import Dashboard
import TableView
import LoginScreen
import SessionModel exposing (..) 
import ClientSession exposing (..) 
import Menu exposing (..)
import Msg exposing (..)


emptyModel : Model
emptyModel =
  { baseUrl = baseUrl
  , place = ME_Dashboard
  , session = Nothing
  , menu = menuDefinition
  , config = (CssConfig (Size 0 0) False)
  , dashboardComponent = Dashboard.component
  , tableComponent = TableView.component
  , loginComponent = LoginScreen.component PlayAsGuest
  , taskComponent = Task.component
  , memberComponent = Member.component
  , reportComponent = Report.component
  }


initModel : Result String MenuEntry -> (Model, Cmd Msg)
initModel url =
  let
    exec = perform (\_ -> (Resize (Size 0 0))) (Resize) Window.size 
    (model, cmd) = urlUpdate url emptyModel
    session = getSession baseUrl GetSession
    place =
      case url of
        Ok entry ->
          entry
        _ ->
          ME_Dashboard
  in
    ({ model | place = place }, Cmd.batch [ exec, session, cmd ])


toUrl : Model -> String
toUrl model =
  let
    newUrl =
      (replace All (regex " +") (\m -> "/") <|
        "#/" ++ (dropLeft 3 <| toString model.place))
  in
    Debug.log "toUrl" newUrl


{-| The URL is turned into a result. If the URL is valid, we just update our
model to the new count. If it is not a valid URL, we modify the URL to make
sense.
-}
urlUpdate : Result String MenuEntry -> Model -> (Model, Cmd Msg)
urlUpdate result model =
  case Debug.log "urlUpdate" result of
    Ok place ->
      case place of
        ME_Task page ->
          setPlaceInnerComponent .taskComponent
            setTaskComponent model place page

        ME_Member page ->
          setPlaceInnerComponent .memberComponent
            setMemberComponent model place page

        ME_Report page ->
          setPlaceInnerComponent .reportComponent
            setReportComponent model place page
          
        ME_Dashboard ->
          setPlace (Context Dashboard) .dashboardComponent model place
          
        ME_Table name ->
          setPlace (Context Table) .tableComponent model place

    Err errorMsg ->
      let
        error = Debug.log "Error message" errorMsg
      in
        ({ model | place = ME_Dashboard }, Navigation.modifyUrl "#/Dashboard")
