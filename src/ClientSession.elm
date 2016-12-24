module ClientSession exposing (..)

import Date exposing (Date, fromString)
import Json.Decode as Json exposing (..)
import Result exposing (toMaybe)
import Task


import ClientApi exposing (..)
import Rest exposing (..)
import SessionModel exposing (..)


getSession : String -> (RestResult Session -> msg) -> Cmd msg
getSession baseUrl msg =
    get "" sessions
        |> Task.attempt msg


createGuestSession : String -> (RestResult Session -> msg) -> Cmd msg
createGuestSession baseUrl msg =
    get "guest" sessions
        |> Task.attempt msg


dateParser : Maybe String -> Maybe Date
dateParser input =
    case input of
        Just str ->
            str |> fromString >> toMaybe

        Nothing ->
            Nothing


