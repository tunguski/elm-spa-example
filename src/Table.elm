module Table exposing (tablesApiPart)

import ApiPartApi exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (..)
import Http exposing (Error)
import MongoDb exposing (..)
import Rest exposing (..)
import Server exposing (..)
import SessionModel exposing (Session)
import String
import Task exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import UrlParse exposing (..)


tablesApiPart :
    ApiPartApi msg
    -> Parse (Partial msg)
tablesApiPart api =
    P "tables"
        [ S
            (\id ->
                [ F
                    (\() ->
                        api.doWithSession
                            (\session ->
                                get id games
                                    |> andThen
                                        (\table ->
                                            table |> (encode gameEncoder >> okResponse >> Task.succeed)
                                        )
                                    |> onError
                                        (\error ->
                                            let
                                                x =
                                                    Debug.log "error" error
                                            in
                                            statusResponse 404 |> Task.succeed
                                        )
                            )
                    )
                ]
            )
        , F
            (\() ->
                case api.request.method of
                    Get ->
                        api.doWithSession
                            (\session ->
                                listDocuments games
                                    |> andThen
                                        (encodeCollection gameEncoder
                                            >> okResponse
                                            >> Task.succeed
                                        )
                                    |> onError (Debug.toString >> response 500 >> Task.succeed)
                            )

                    _ ->
                        statusResponse 405 |> Result
            )
        ]
