module Main exposing (..)

import Navigation
import String exposing (dropLeft)
import UrlParser exposing (..)
import Window exposing (..)
import Layout exposing (view, componentSubsMap)
import Msg exposing (..)
import Model exposing (..)
import ModelOps exposing (initModel, locationToMsg)
import Update exposing (update)
import Task.Task as Task
import Member.Member as Member
import Report.Report as Report


main =
    Navigation.program
        locationToMsg 
        { init = initModel
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ resizes Resize
        , componentSubsMap model
        ]


