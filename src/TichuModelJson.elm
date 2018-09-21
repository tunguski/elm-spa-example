module TichuModelJson exposing (awaitingTableDecoder, awaitingTableEncoder, card, cardEncoder, cardsDecoder, decodeCards, decodeString, encodeAwaitingTable, encodeCards, encodeGame, encodeGameConfig, encodeSuit, gameConfigDecoder, gameConfigEncoder, gameDecoder, gameEncoder, gameUpdate, gameUser, gameUserEncoder, maybeEncoder, message, messageEncoder, messageType, player, playerEncoder, rank, rankEncoder, round, roundEncoder, suit)

import BaseModel exposing (..)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import String exposing (toInt)
import TichuModel exposing (..)
import Time 
import Tuple
import UserModel exposing (..)


gameConfigDecoder : Decoder GameConfig
gameConfigDecoder =
    Json.map GameConfig
        (field "gameType" string
            |> andThen
                (\string ->
                    case string of
                        "Humans" ->
                            succeed Humans

                        "HumansVsBots" ->
                            succeed HumansVsBots

                        "Bots" ->
                            succeed Bots

                        _ ->
                            fail "Unknown game type"
                )
        )


gameDecoder : Decoder Game
gameDecoder =
    Json.map8 Game
        (field "name" string)
        (field "config" gameConfigDecoder)
        (field "test" bool)
        (field "seed" longInt)
        (field "users" <| list gameUser)
        (field "round" round)
        (field "history" <| list round)
        (field "messages" <| list message)
        |> andThen
            (\p ->
                Json.map2 p
                    (field "log" <| list gameUpdate)
                    (field "finished" <| maybe (list int))
            )


round : Decoder Round
round =
    Json.map8 Round
        (field "players" <| list player)
        (field "table" <| list cardsDecoder)
        (field "actualPlayer" int)
        (field "tableHandOwner" (maybe int))
        (field "demand" (maybe rank))
        (field "demandCompleted" bool)
        (field "seed" longInt)
        (field "winner" (maybe int))


gameUser : Decoder GameUser
gameUser =
    Json.map3 GameUser
        (field "name" string)
        (field "lastCheck" longInt)
        (field "human" bool)


cardsDecoder : Decoder Cards
cardsDecoder =
    list card


stringifyError : Result Error Cards -> Result String Cards
stringifyError input =
    case input of
        Ok a ->
            Ok a

        Err a ->
            Err (Debug.toString a)


decodeCards : String -> Result String Cards
decodeCards =
    decodeString cardsDecoder
        >> stringifyError


decodeString =
    Json.decodeString


card : Decoder Card
card =
    field "type" string
        |> andThen
            (\card_ ->
                case card_ of
                    "NormalCard" ->
                        map2
                            (\suit_ rank_ -> NormalCard suit_ rank_)
                            suit
                            rank

                    "MahJong" ->
                        succeed MahJong

                    "Dog" ->
                        succeed Dog

                    "Phoenix" ->
                        succeed Phoenix

                    "Dragon" ->
                        succeed Dragon

                    _ ->
                        fail ("Not valid pattern for decoder to Card. Pattern: " ++ Debug.toString string)
            )


suit : Decoder Suit
suit =
    field "suit" string
        |> andThen
            (\string ->
                case string of
                    "Hearts" ->
                        succeed Hearts

                    "Diamonds" ->
                        succeed Diamonds

                    "Spades" ->
                        succeed Spades

                    "Clubs" ->
                        succeed Clubs

                    _ ->
                        fail ("Not valid pattern for decoder to Suit. Pattern: " ++ Debug.toString string)
            )


rank : Decoder Rank
rank =
    field "rank" string
        |> andThen
            (\string ->
                case string of
                    "J" ->
                        succeed J

                    "Q" ->
                        succeed Q

                    "K" ->
                        succeed K

                    "A" ->
                        succeed A

                    _ ->
                        case toInt string of
                            Just i ->
                                succeed (R i)

                            _ ->
                                fail ("Not valid pattern for decoder to Rank. Pattern: " ++ string)
            )


player : Decoder Player
player =
    map7 Player
        (field "hand" <| cardsDecoder)
        (field "cardsOnHand" <| int)
        (field "collected" <| list (list cardsDecoder))
        (field "name" string)
        (field "score" int)
        (field "tichu" bool)
        (field "sawAllCards" bool)
        |> andThen
            (\p ->
                map2 p
                    (field "grandTichu" bool)
                    (field "exchange"
                        (maybe
                            (cardsDecoder
                                |> andThen
                                    (\list ->
                                        case list of
                                            a :: b :: c :: t ->
                                                succeed ( a, b, c )

                                            _ ->
                                                fail "Could not parse list with three elements"
                                    )
                            )
                        )
                    )
            )


message : Decoder Message
message =
    map2 Message
        (field "type" messageType)
        (field "text" string)


messageType : Decoder MessageType
messageType =
    string
        |> andThen
            (\string ->
                case string of
                    "Error" ->
                        succeed Error

                    "Warning" ->
                        succeed Warning

                    "Info" ->
                        succeed Info

                    "Success" ->
                        succeed Success

                    _ ->
                        fail ("Not valid pattern for decoder to MessageType. Pattern: " ++ Debug.toString string)
            )


gameUpdate : Decoder UpdateGame
gameUpdate =
    fail "Not implemented yet"


gameEncoder : Game -> Value
gameEncoder game =
    JE.object
        [ ( "name", JE.string game.name )
        , ( "config", gameConfigEncoder game.config )
        , ( "test", JE.bool game.test )
        , ( "seed", JE.string <| Debug.toString game.seed )
        , ( "users", JE.list gameUserEncoder game.users )
        , ( "round", roundEncoder game.round )
        , ( "history", JE.list roundEncoder game.history )
        , ( "messages", JE.list messageEncoder game.messages )
        , ( "log", JE.list JE.string [] )
        , ( "finished"
          , case game.finished of
                Just list ->
                    JE.list JE.int list

                _ ->
                    JE.null
          )
        ]


gameConfigEncoder : GameConfig -> Value
gameConfigEncoder config =
    JE.object
        [ ( "gameType", JE.string <| Debug.toString config.gameType )
        ]


encodeGameConfig config =
    JE.encode 0 <| gameConfigEncoder config


gameUserEncoder : GameUser -> Value
gameUserEncoder gameUser_ =
    JE.object
        [ ( "name", JE.string gameUser_.name )
        , ( "lastCheck", JE.int <| gameUser_.lastCheck )
        , ( "human", JE.bool gameUser_.human )
        ]


messageEncoder : Message -> Value
messageEncoder message_ =
    JE.object
        []


cardEncoder : Card -> Value
cardEncoder card_ =
    case card_ of
        NormalCard suit_ rank_ ->
            JE.object
                [ ( "type", JE.string "NormalCard" )
                , ( "suit", JE.string <| Debug.toString suit_ )
                , ( "rank", rankEncoder rank_ )
                ]

        _ ->
            JE.object
                [ ( "type", JE.string (Debug.toString card) ) ]


encodeCards : Cards -> String
encodeCards cards =
    JE.encode 0 <| JE.list cardEncoder cards


rankEncoder : Rank -> Value
rankEncoder rank_ =
    JE.string <|
        case rank_ of
            R value ->
                Debug.toString value

            _ ->
                Debug.toString rank_


playerEncoder : Player -> Value
playerEncoder player_ =
    JE.object
        [ ( "hand", listToValue cardEncoder player_.hand )
        , ( "cardsOnHand", JE.int player_.cardsOnHand )
        , ( "collected", listToValue (listToValue (listToValue cardEncoder)) player_.collected )
        , ( "name", JE.string player_.name )
        , ( "score", JE.int player_.score )
        , ( "tichu", JE.bool player_.tichu )
        , ( "sawAllCards", JE.bool player_.sawAllCards )
        , ( "grandTichu", JE.bool player_.grandTichu )
        , ( "exchange"
          , case player_.exchange of
                Just ( a, b, c ) ->
                    listToValue cardEncoder [ a, b, c ]

                Nothing ->
                    JE.null
          )
        ]


maybeEncoder maybe encoder =
    case maybe of
        Just value ->
            encoder value

        Nothing ->
            JE.null


roundEncoder : Round -> Value
roundEncoder round_ =
    JE.object
        [ ( "players", listToValue playerEncoder round_.players )
        , ( "table", listToValue (JE.list cardEncoder) round_.table )
        , ( "actualPlayer", JE.int round_.actualPlayer )
        , ( "tableHandOwner", maybeEncoder round_.tableHandOwner JE.int )
        , ( "demand", maybeEncoder round_.demand rankEncoder )
        , ( "demandCompleted", JE.bool round_.demandCompleted )
        , ( "seed", JE.string <| Debug.toString round_.seed )
        , ( "winner", maybeEncoder round_.winner JE.int )
        ]


encodeGame : Game -> String
encodeGame game =
    JE.encode 0 <| gameEncoder game


encodeSuit : Suit -> Value
encodeSuit item =
    case item of
        Hearts ->
            JE.string "Hearts"

        Diamonds ->
            JE.string "Diamonds"

        Spades ->
            JE.string "Spades"

        Clubs ->
            JE.string "Clubs"


awaitingTableDecoder : Decoder AwaitingTable
awaitingTableDecoder =
    Json.map5 AwaitingTable
        (field "name" string)
        (field "config" gameConfigDecoder)
        (field "users" <|
            list
                (map4 AwaitingTableUser
                    (field "name" string)
                    (field "lastCheck" longInt)
                    (field "pressedStart" bool)
                    (field "human" bool)
                )
        )
        (field "test" bool)
        (field "seed" longInt)


awaitingTableEncoder : AwaitingTable -> Value
awaitingTableEncoder table =
    JE.object
        [ ( "name", JE.string table.name )
        , ( "config", gameConfigEncoder table.config )
        , ( "seed", JE.string <| Debug.toString table.seed )
        , ( "users"
          , JE.list
                (\user ->
                    JE.object
                        [ ( "name", JE.string user.name )
                        , ( "lastCheck", JE.int <|  user.lastCheck )
                        , ( "pressedStart", JE.bool user.pressedStart )
                        , ( "human", JE.bool user.human )
                        ]
                )
                table.users
          )
        , ( "test", JE.bool table.test )
        ]


encodeAwaitingTable awaitingTable =
    JE.encode 0 <| awaitingTableEncoder awaitingTable
