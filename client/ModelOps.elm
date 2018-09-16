module ModelOps exposing (changeLocation, emptyModel, initModel, locationToMsg, processUrlChange, processUrlRequest, toUrl, urlParser)

import Browser.Navigation as Navigation exposing (Key)
import ClientApi exposing (..)
import Component exposing (..)
import Config exposing (..)
import Dashboard
import LoginScreen
import Member.Member as Member
import Menu exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Regex exposing (..)
import Report.Report as Report
import Rest exposing (..)
import SessionModel exposing (..)
import String exposing (dropLeft)
import TableView
import Task
import Task.Task as Task
import Tests
import Url exposing (Url)
import Url.Parser exposing (..)


emptyModel : Key -> Model
emptyModel key =
    { key = key
    , baseUrl = baseUrl
    , place = ME_Dashboard
    , session = Nothing
    , menu = menuDefinition
    , config = CssConfig ( 1366, 768 ) False
    , dashboardComponent = Dashboard.component key
    , tableComponent = TableView.component "noone" "nonexistent"
    , loginComponent = LoginScreen.component PlayAsGuest
    , taskComponent = Task.component
    , memberComponent = Member.component
    , reportComponent = Report.component
    , testsComponent = Tests.component
    }


initModel : () -> Url -> Key -> ( Model, Cmd Msg )
initModel _ url key =
    let
        --    exec =
        --        Task.perform Resize Window.size
        session =
            get "" sessions
                |> Task.attempt GetSession

        parsed =
            Url.Parser.parse urlParser url

        ( model, cmd ) =
            changeLocation parsed (emptyModel key)
    in
    ( model
    , Cmd.batch [ session, cmd ]
    )



-- URL PARSERS


urlParser : Parser (MenuEntry -> a) a
urlParser =
    oneOf
        [ map ME_Dashboard (s "Dashboard")
        , map ME_Tests (s "Tests")
        , map ME_Table (s "Table" </> string)
        , map ME_Task
            (s "Task"
                </> oneOf
                        [ map Task.Search (s "Search")
                        , map Task.Add (s "Add")
                        ]
            )
        , map ME_Member
            (s "Member"
                </> oneOf
                        [ map Member.Search (s "Search")
                        , map Member.Add (s "Add")
                        ]
            )
        , map ME_Report
            (s "Report"
                </> oneOf
                        [ map Report.Search (s "Search")
                        , map Report.Add (s "Add")
                        ]
            )
        ]


processUrlRequest location model =
    ( model, Cmd.none )


processUrlChange location model =
    ( model, Cmd.none )


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
                                        { model | tableComponent = TableView.component "noone" name }
                        )
                        place

        Nothing ->
            ( { model | place = ME_Dashboard }, Navigation.pushUrl model.key "#/Dashboard" )


locationToMsg location =
    Url.Parser.parse urlParser location
        |> UrlUpdate


toUrl : Model -> String
toUrl model =
    let
        newUrl =
            case Regex.fromString " +" of
                Just r ->
                    replace r (\match -> "/") <|
                        "#/"
                            ++ (dropLeft 3 <| Debug.toString model.place)

                _ ->
                    "#/error!"
    in
    Debug.log "toUrl" newUrl
