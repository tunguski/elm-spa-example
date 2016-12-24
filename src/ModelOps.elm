module ModelOps exposing (..)

import Navigation exposing (Location)
import Task
import Window exposing (Size)
import Regex exposing (..)
import String exposing (dropLeft)
import UrlParser exposing (..)


import Config exposing (..)
import Model exposing (..)
import Component exposing (..)
import Task.Task as Task
import Member.Member as Member
import Report.Report as Report
import Dashboard
import TableView
import Tests
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
    , tableComponent = TableView.component "nonexistent"
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

        ( model, cmd ) =
            emptyModel ! [] 

        session =
            getSession baseUrl GetSession

        parsed =
          parseHash urlParser url 

        place =
            case parsed of
                Just entry ->
                    entry

                _ ->
                    ME_Dashboard
    in
        ( { model | place = place }, Cmd.batch [ exec, session, cmd ] )


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


