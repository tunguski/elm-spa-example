module TichuModel exposing (..)

import Array
import Time exposing (Time)
import List exposing (..)
import Maybe exposing (andThen)
import Random exposing (initialSeed, step)
import Random.List exposing (shuffle)


import UserModel exposing (User)


-----------------------------------------------------------------------------
-- MODEL
-----------------------------------------------------------------------------


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type Rank
    = R Int
    | J
    | Q
    | K
    | A


type Card
    = NormalCard Suit Rank
    | MahJong
    | Dog
    | Phoenix
    | Dragon


type alias Cards =
    List Card


allowedRanks : List Rank
allowedRanks =
    (map (\i -> R i) (List.range 2 10)) ++ [ J, Q, K, A ]


rankWeight : Rank -> Int
rankWeight rank =
    case rank of
        R i -> i
        J -> 11
        Q -> 12
        K -> 13
        A -> 14


cardWeight : Card -> Int
cardWeight card =
    case card of
        NormalCard suit rank -> rankWeight rank
        MahJong -> 1
        Dog -> 0
        Phoenix -> 15
        Dragon -> 16


cardOrder : Card -> Card -> Order
cardOrder a b =
    compare (cardWeight a) (cardWeight b)


allCards : List Card
allCards =
    append [ MahJong, Dog, Phoenix, Dragon ]
        (concatMap (\s -> map (NormalCard s) allowedRanks) [ Clubs, Diamonds, Hearts, Spades ])


type
    Combination
    -- lowest, length; (bomb)
    = StraightFlush Rank Int
      -- (bomb)
    | Four Rank
      -- three's rank
    | FullHouse Rank
      -- lowest, length
    | Straight Rank Int
    | Three Rank
      -- lowest, length
    | PairStairs Rank Int
    | Pair Rank
      -- card's power
    | SingleCard Int



-- a single card;


highCard : Cards -> Maybe Combination
highCard combination =
    case combination of
        [ a ] ->
            Just (SingleCard (cardWeight a))

        _ ->
            Nothing


pair : Cards -> Maybe Combination
pair combination =
    case combination of
        [ NormalCard a b, NormalCard c d ] ->
            if rankWeight b == rankWeight d then
                Just (Pair b)
            else
                Nothing

        _ ->
            Nothing



-- two or more "stairs" (consecutive pairs; for example, 55667788. Non-consecutive pairs may not be played);


pairStairs : Cards -> Maybe Combination
pairStairs combination =
    Maybe.map (\( r, i ) -> PairStairs r i) (extractPairStairs combination)


nextPairPower : ( Rank, Int ) -> Card -> ( Rank, Int )
nextPairPower ( r, i ) card =
    case card of
        NormalCard s r ->
            ( r, i + 1 )

        _ ->
            ( r, i )


calculatePairStraitPower : Card -> Card -> Maybe Card
calculatePairStraitPower a b =
    ((pair [ a, b ])
        |> andThen
            (\combination ->
                case combination of
                    Pair rank ->
                        case a of
                            NormalCard s r ->
                                if ((rankWeight r) == 0 || (rankWeight r) == (rankWeight rank) + 1) then
                                    Just a
                                else
                                    Nothing

                            _ ->
                                Nothing

                    _ ->
                        Nothing
            )
    )


extractPairStairs : Cards -> Maybe ( Rank, Int )
extractPairStairs combination =
    case combination of
        a :: b :: tail ->
            Maybe.map2 nextPairPower
                (extractPairStairs tail)
                (calculatePairStraitPower a b)

        _ ->
            Nothing



-- three of a kind;
-- straights of at least five cards in length, regardless of suit/color (so 56789TJQ is playable);
-- and full houses (three of a kind & a pair).
-- Four of a kind or a straight flush of at least five cards is a bomb


allowedCombination : Cards -> Cards -> Bool
allowedCombination table combination =
    False


parseTrick : Cards -> Maybe Combination
parseTrick cards =
    Nothing


cardInTrick : Maybe Rank -> Cards -> Bool
cardInTrick rank selection =
    case rank of
        Just r ->
            List.any (\card ->
                case card of
                    NormalCard suit cardRank ->
                        cardRank == r
                    _ ->
                        False
            ) selection
        Nothing ->
            True


bomb : Maybe Combination -> Bool
bomb combination =
    case combination of
        Just (StraightFlush r i) ->
            True
        Just (Four r) ->
            True
        _ ->
            False



type alias Player =
    { hand : List Card
    , cardsOnHand : Int
    , collected : List (List Cards)
    , selection : List Card
    , name : String
    , score : Int
    , tichu : Bool
    , sawAllCards : Bool
    , grandTichu : Bool
    , exchange : Maybe (Card, Card, Card)
    }


type alias Round =
    -- players in order, first has to play
    { players : List Player
    -- hands on table
    , table : List Cards
    , actualPlayer : Int
    -- the owner of actually highest trick on table
    , tableHandOwner : Maybe Int
    , demand : Maybe Rank
    , demandCompleted : Bool
    , seed : Int
    }


type MessageType
    = Error
    | Warning
    | Info
    | Success


type alias Message =
    { messageType : MessageType
    , text : String
    }


type alias GameUser =
    { name : String
    , lastCheck : Time
    }


type alias Game =
    { name : String
    , seed : Int
    , users : List GameUser
    , round : Round
    , history : List Round
    , messages : List Message
    , log : List UpdateGame
    }


type alias AwaitingTableUser =
    { name : String
    , lastCheck : Time
    , pressedStart : Bool
    }


type alias AwaitingTable =
    { name : String
    , users : List AwaitingTableUser
    , test : Bool
    , seed : Int
    }


type UpdateGame
    = UpdatePlayer (Player -> Player)
    | UpdateRound (Round -> Round)


initGame : String -> Int -> List AwaitingTableUser -> Game
initGame name seed users =
    let
        gameUsers =
            List.map (\user ->
                GameUser user.name user.lastCheck
            ) users
    in
        { name = name
        , seed = seed
        , users = gameUsers
        , round = initRound seed gameUsers
        , history = []
        , messages = []
        , log = []
        }


hasCard card player =
    List.member card player.hand


initRound : Int -> List GameUser -> Round
initRound seed users =
    case step (shuffle allCards) (initialSeed seed) of
        (cards, _) ->
            { players =
                List.indexedMap (\i user ->
                    initPlayer cards user.name i
                ) users
            , table = []
            , actualPlayer = 0
            , tableHandOwner = Nothing
            , demand = Nothing
            , demandCompleted = False
            , seed = seed
            }
    -- set actual player by seeking MahJong
    |> (\round ->
        { round | actualPlayer =
            List.indexedMap (,) round.players
            |> List.foldl (\(i, user) before ->
                case hasCard MahJong user of
                    True -> Just i
                    False -> before
            ) Nothing
            |> Maybe.withDefault 0
        }
    )


initPlayer : List Card -> String -> Int -> Player
initPlayer cards name offset =
    { hand = {-sortWith cardOrder <|-}
        take 14 <| drop (offset * 14) cards
    , cardsOnHand = 14
    , collected = []
    , selection = []
    , name = name
    , score = 0
    , tichu = False
    , sawAllCards = False
    , grandTichu = False
    , exchange = Nothing
    }


openDemand round =
    case round.demand of
        Just r -> not round.demandCompleted
        Nothing -> False


openDemandMatch round =
    case round.demand of
        Just r ->
            List.any (\card ->
                case card of
                    NormalCard suit rank ->
                        (not round.demandCompleted) && (rank == r)
                    _ ->
                        False
            ) (getActualPlayer round).hand
        Nothing ->
            False


defaultCrash text item =
    case item of
        Just value -> value
        Nothing -> Debug.crash text


getPlayer round name =
    List.filter (.name >> (==) name) round.players
    |> List.head
    |> defaultCrash ("Malformed state getPlayer: " ++ name ++ ": " ++ toString round)


modifyPlayer name function round =
    { round | players =
        List.map (\player ->
            case player.name == name of
                True -> function player
                False -> player
        ) round.players
    }


getActualPlayer : Round -> Player
getActualPlayer round =
    round.players
    |> List.drop round.actualPlayer
    |> List.head
    |> defaultCrash ("Malformed state getActualPlayer: " ++ toString round)


incActualPlayer round =
    { round | actualPlayer = (round.actualPlayer + 1) % 4 }
    |> (\round ->
        if List.isEmpty (getActualPlayer round).hand then
            incActualPlayer round
        else
            round
    )


removeCards cards hand =
    List.filter (\c -> not <| List.member c cards) hand


switchCard card i players =
    (Array.get (i % 4) players
     |> (\pl ->
         case pl of
             Just player ->
                Array.set (i % 4)
                    { player | hand = card :: player.hand }
                    players
             Nothing ->
                 players
    ))


setTableHandOwnerAsActualPlayer owner round =
    { round | tableHandOwner = Just <| Debug.log "tableHandOwner" owner }


putCardsOnTable cards round =
    { round | table = cards :: round.table }


nextRound table =
    { table
    | round = initRound ((table.round.seed + 19) * 263) table.users
    , history = table.round :: table.history
    }


maybeEndRound table =
    table.round.players
    |> List.filter (.hand >> List.isEmpty)
    |> List.length
    |> (\activePlayers ->
        if (activePlayers == 1) then
            nextRound table
        else
            table
    )


modifyNthPlayer i modifier round =
    case List.drop i round.players |> List.head of
        Just player ->
            modifyPlayer player.name modifier round
        Nothing ->
            Debug.log ("Could not find nth player: " ++ toString i) round


maybeCollectCards round =
    case round.tableHandOwner of
        Just owner ->
            if owner == round.actualPlayer then
                collectCards owner round
            else
                round
        Nothing ->
            round


collectCards owner round =
    { round
    | tableHandOwner = Nothing
    , table = []
    }
    |> modifyNthPlayer owner
        (\player -> { player
            | collected = round.table :: player.collected
        })


{-| Exchange cards between players
-}
exchangeCardsBetweenPlayers table =
    Array.fromList table.round.players
    |> (\p ->
        Array.indexedMap (,) p
        |> Array.foldr (\(i, player) players ->
            case player.exchange of
                Just (a, b, c) ->
                    switchCard c (i + 1) players
                    |> switchCard a (i + 2)
                    |> switchCard b (i + 3)
                Nothing ->
                    Debug.log "Cannot exchange if player did not declare cards" players
        ) p
    )
    |> (\players ->
        let
            round = table.round
        in
            { round
            | players = List.map (\player ->
                    { player | hand =
                        case player.exchange of
                            Just (a, b, c) ->
                                removeCards [a, b, c] player.hand
                                |> sortWith cardOrder
                            Nothing ->
                                Debug.log "Cannot remove if player did not declare cards" player.hand
                    }
                ) <| Array.toList players
            }
    )
    |> (\round -> { table | round = round })


