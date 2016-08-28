module Update exposing (..)


import Navigation 
import Task 


import Model exposing (..)
import ModelOps exposing (..)
import Component exposing (Component, Context)
import LoginScreen


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    None -> model ! []

    GetSession result ->
      case (Debug.log "getSession" result) of
        Ok session ->
          { model | session = Just session } ! 
            [ Navigation.modifyUrl "#/Dashboard"
            ]
        Err error ->
          { model | session = Nothing } ! []

    InitialWindowSize -> model ! []

    ToggleHamburgerMenu ->
      let
        menu = model.menu
      in
        { model | menu = { menu | expanded = not menu.expanded } } ! []

    Resize size ->
      let
        config = model.config
      in
        { model | config = { config | windowSize = size } } ! []

    ToggleSideMenu shorten -> 
      let
        config = model.config
      in
        { model | config = { config | smallSidebar = shorten } } ! []

    ChangeView entry ->
      let
        menu = model.menu
        newModel = { model 
          | place = entry
          , menu = { menu | expanded = False }
        }
      in
        newModel ! [ Navigation.newUrl (toUrl newModel) ]

    Dashboard msg ->
      Component.updateModel (Context Dashboard) .dashboardComponent
        setDashboardComponent model msg

    Login msg ->
      case msg of
        LoginScreen.GetSession result ->
          model ! [ Task.perform (Err >> GetSession) (GetSession) <| Task.succeed result ]
        _ ->
          Component.updateModel (Context Login) .loginComponent
            setLoginComponent model msg

    Task msg ->
      Component.updateModel (Context Task) .taskComponent
        setTaskComponent model msg

    Member msg ->
      Component.updateModel (Context Member) .memberComponent
        setMemberComponent model msg

    Report msg ->
      Component.updateModel (Context Report) .reportComponent
        setReportComponent model msg


