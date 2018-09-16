module ClientMain exposing (main, subscriptions)

import Browser
import Browser.Events exposing (onResize)
import Layout exposing (componentSubsMap, view)
import Model exposing (..)
import ModelOps exposing (initModel, locationToMsg)
import Msg exposing (..)
import String exposing (dropLeft)
import Update exposing (update)
import Url.Parser exposing (..)


main =
    Browser.application
        { init = initModel
        , update = update
        , subscriptions = subscriptions
        , view = view

        --locationToMsg
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize Resize
        , componentSubsMap model
        ]
