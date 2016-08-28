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


dateParser : Maybe String -> Maybe Date
dateParser input =
  case input of
    Just str ->
      str |> fromString >> toMaybe
    Nothing -> 
      Nothing



sessionDecoder : Decoder Session
sessionDecoder =
  Json.object5 Session
    ("username" := string)
    ("token" := string)
    (map dateParser <| maybe <| "loginTime" := string)
    (map dateParser <| maybe <| "lastRequestTime" := string)
    ("idUser" := string)
