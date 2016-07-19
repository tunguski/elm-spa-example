module ClientSession exposing (..)


import Date exposing (Date, fromString)
import Json.Decode as Json exposing (..)
import Result exposing (toMaybe)
import Http
import Task


type alias Session =
  { username : String
  , token : String
  , loginTime : Maybe Date
  , lastRequestTime : Maybe Date
  , idUser : String
  }


getSession : String -> (Result Http.Error Session -> msg) -> Cmd msg
getSession baseUrl msg =
  Http.get sessionDecoder 
  (baseUrl ++ "session")
    |> Task.perform Err Ok
    |> Cmd.map msg 


createGuestSession : String -> (Result Http.Error Session -> msg) -> Cmd msg
createGuestSession baseUrl msg =
  Http.get sessionDecoder 
  (baseUrl ++ "session/guest")
    |> Task.perform Err Ok
    |> Cmd.map msg 


dateParser : String -> Maybe Date
dateParser input =
  input |> fromString >> toMaybe



sessionDecoder : Decoder Session
sessionDecoder =
  Json.object5 Session
    ("username" := string)
    ("token" := string)
    (map dateParser <| "loginTime" := string)
    (map dateParser <| "lastRequestTime" := string)
    ("idUser" := string)
