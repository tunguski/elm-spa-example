module Main exposing (..)

import Navigation 
import String exposing (dropLeft) 
import UrlParser exposing (..) 
import Window exposing (..)

import Layout exposing (view)
import Msg exposing (..)
import Model exposing (..)
import ModelOps exposing (initModel, urlUpdate)
import Update exposing (update)

import Task.Task as Task
import Member.Member as Member
import Report.Report as Report


main =
  Navigation.program
    urlParser
    { init = initModel
    , urlUpdate = urlUpdate
    , update = update
    , subscriptions = subscriptions 
    , view = view
    }


-- URL PARSERS


urlParser : Navigation.Parser (Result String MenuEntry)
urlParser =
  let
    parser : Parser (MenuEntry -> a) a
    parser = 
      oneOf
        [ format ME_Dashboard (s "Dashboard")
        , format ME_Task (s "Task" </> (
          oneOf
            [ format Task.Search (s "Search")
            , format Task.Add (s "Add")
            ]
          ))
        , format ME_Member (s "Member" </> (
          oneOf
            [ format Member.Search (s "Search")
            , format Member.Add (s "Add")
            ]
          ))
        , format ME_Report (s "Report" </> (
          oneOf
            [ format Report.Search (s "Search")
            , format Report.Add (s "Add")
            ]
          ))
        ]
  in
    Navigation.makeParser (\location ->
      parse identity parser (dropLeft 2 location.hash))


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ resizes Resize
  ]


