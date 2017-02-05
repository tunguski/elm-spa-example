module ModelOps exposing (..)

import Navigation exposing (Location)
import Task
import Window exposing (Size)
import Regex exposing (..)
import String exposing (dropLeft)
import UrlParser exposing (..)


import ClientApi exposing (..)
import Component exposing (..)
import Config exposing (..)
import Dashboard
import LoginScreen
import Member.Member as Member
import Menu exposing (..)
import Msg exposing (..)
import Model exposing (..)
import Report.Report as Report
import Rest exposing (..)
import SessionModel exposing (..)
import Task.Task as Task
import TableView
import Tests


emptyModel : Model
emptyModel =
    { baseUrl = baseUrl
    , place = ME_Dashboard
    , session = Nothing
    , menu = menuDefinition
    , config = (CssConfig (Size 1366 768) False)
    , dashboardComponent = Dashboard.component
    , tableComponent = TableView.component "noone" "nonexistent"
    , loginComponent = LoginScreen.component PlayAsGuest
    , taskComponent = Task.component
    , memberComponent = Member.component
    , reportComponent = Report.component
    , testsComponent = Tests.component
    }


initModel : Location -> ( Model, Cmd Msg )
initModel url =
    let
        exec =
            Task.perform (Resize) Window.size

        session =
            get "" sessions
            |> Task.attempt GetSession

        parsed =
          parseHash urlParser url

        ( model, cmd ) =
            changeLocation parsed emptyModel
    in
        model ! [ exec, session, cmd ]


-- URL PARSERS


urlParser : Parser (MenuEntry -> a) a
urlParser =
    oneOf
        [ map ME_Dashboard (s "Dashboard")
        , map ME_Tests (s "Tests")
        , map ME_Table (s "Table" </> string)
        , map ME_Task
            (s "Task"
                </> (oneOf
                        [ map Task.Search (s "Search")
                        , map Task.Add (s "Add")
                        ]
                    )
            )
        , map ME_Member
            (s "Member"
                </> (oneOf
                        [ map Member.Search (s "Search")
                        , map Member.Add (s "Add")
                        ]
                    )
            )
        , map ME_Report
            (s "Report"
                </> (oneOf
                        [ map Report.Search (s "Search")
                        , map Report.Add (s "Add")
                        ]
                    )
            )
        ]


changeLocation location model =
  case location of
      Just place ->
          case place of
              ME_Task page ->
                  setPlaceInnerComponent .taskComponent
                      setTaskComponent
                      model
                      place
                      page

              ME_Member page ->
                  setPlaceInnerComponent .memberComponent
                      setMemberComponent
                      model
                      place
                      page

              ME_Report page ->
                  setPlaceInnerComponent .reportComponent
                      setReportComponent
                      model
                      place
                      page

              ME_Tests ->
                  setPlace (Context Tests) .testsComponent model place

              ME_Dashboard ->
                  setPlace (Context Dashboard) .dashboardComponent model place

              ME_Table name ->
                  setPlace (Context Table)
                      .tableComponent
                      (case name == model.tableComponent.model.name of
                          True ->
                              model

                          False ->
                              -- create new component on each url change
                              case model.session of
                                    Just session ->
                                        { model | tableComponent = TableView.component session.username name }
                                    Nothing ->
                                        model
                      )
                      place

      Nothing ->
          ( { model | place = ME_Dashboard }, Navigation.modifyUrl "#/Dashboard" )


locationToMsg location =
  parseHash urlParser location
  |> UrlUpdate


toUrl : Model -> String
toUrl model =
    let
        newUrl =
            (replace All (regex " +") (\m -> "/") <|
                "#/"
                    ++ (dropLeft 3 <| toString model.place)
            )
    in
        Debug.log "toUrl" newUrl


