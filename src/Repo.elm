module Repo exposing (RepoInfo, repoInfoDecoder)

import BaseModel exposing (Collection)
import Http exposing (Error)
import Json.Decode as Json exposing (..)
import Task exposing (Task)


type alias RepoInfo =
    { id : String
    , name : String
    }


repoInfoDecoder : Json.Decoder RepoInfo
repoInfoDecoder =
    Json.map2 RepoInfo
        (at [ "_id" ] <| field "$oid" string)
        (field "name" string)



--getRepoInfos : Task Error (Collection RepoInfo)
--getRepoInfos =
--    db.listDocuments repoInfoDecoder "coll"
