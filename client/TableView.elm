module TableView exposing
    ( Model
    , Msg(..)
    , awaitingTableView
    , chatPanel
    , clearSelection
    , component
    , emptyModel
    , exchangeButton
    , exchangeCard
    , gameButton
    , gameSummaryPanel
    , gameView
    , getTable
    , grandTichuButton
    , init
    , isGivingDragon
    , oldTichuView
    , orElse
    , passButton
    , playButton
    , playerBox
    , playerGameBox
    , printCard
    , printCardSkeleton
    , printCards
    , printExchangeCard
    , printHiddenCards
    , printRow
    , printTableHand
    , seeAllCardsButton
    , sendCommand
    , sendCustomCommand
    , setSelection
    , showPlayer
    , showRound
    , startGame
    , styleProperCombination
    , tichuButton
    , update
    , view
    )

import BaseModel exposing (..)
import Browser.Navigation
import ClientApi exposing (..)
import Common exposing (..)
import Component exposing (..)
import Config exposing (..)
import Css exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import Murmur3
import Process
import Rest exposing (..)
import Result exposing (toMaybe)
import Task exposing (Task)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import Time exposing (Posix, every, now)


component : String -> String -> Component Model msg Msg
component userName tableName =
    Component (emptyModel userName tableName)
        update
        view
        (Just (init tableName))
        Nothing



-- MODEL


type alias Model =
    { userName : String
    , name : String
    , awaitingTable : Maybe AwaitingTable
    , game : Maybe Game
    , selection : List Card
    , possibilities : List Combination
    , phoenixMeaning : Maybe String
    , demand : Maybe Rank
    , exchange : Dict Int Card
    }


emptyModel : String -> String -> Model
emptyModel userName tableName =
    Model userName tableName Nothing Nothing [] [] Nothing Nothing Dict.empty


init : String -> Context msg Msg -> Cmd msg
init name ctx =
    (get name awaitingTables |> Task.map Ok)
        |> Task.onError (\error -> get name games |> Task.map Err)
        |> Task.attempt UpdateTables
        |> Cmd.map ctx.mapMsg



--getTable : String -> Int -> Context msg Msg -> Cmd msg


getTable name hash ctx =
    games
        |> withQueryParams [ ( "hash", Debug.toString hash ) ]
        |> get name
        |> Task.map Err


startGame : String -> (RestResult String -> msg) -> Cmd msg
startGame name msg =
    postCommand (name ++ "/start") awaitingTables
        |> Task.attempt msg



-- UPDATE


type Msg
    = UpdateTables (RestResult (Result Game AwaitingTable))
    | CheckCard Card
    | DeclareTichu
    | DeclareGrandTichu
    | MahJongRequest (Maybe Rank)
    | ChoosePhoenixMeaning String
    | PlaceCombination
    | SendHand (RestResult String)
    | Pass
    | SeeAllCards
    | Start
    | SentStart (RestResult String)
    | CommandResult (RestResult String)
    | Exchange
    | SetExchange Int
    | PlayerClicked String


update : ComponentUpdate Model msg Msg
update ctx action model =
    case action of
        UpdateTables result ->
            let
                sendGetGame game =
                    let
                        gameHashBase =
                            { game
                                | users =
                                    List.map
                                        (\user ->
                                            { user
                                                | lastCheck = Time.millisToPosix 0
                                            }
                                        )
                                        game.users
                            }

                        hash =
                            Murmur3.hashString 17 (Debug.toString gameHashBase)
                    in
                    [ Process.sleep 500.0
                        |> Task.andThen (\_ -> getTable model.name hash ctx)
                        |> Task.attempt UpdateTables
                        |> Cmd.map ctx.mapMsg
                    ]
            in
            case result of
                Ok res ->
                    let
                        newModel =
                            { model
                                | game = Nothing
                                , awaitingTable = Nothing
                            }
                    in
                    case res of
                        Ok awaitingTable ->
                            ( { newModel | awaitingTable = Just awaitingTable }
                            , Cmd.none
                            )

                        Err game ->
                            ( setSelection { newModel | game = Just game } game model.selection
                            , Cmd.batch (sendGetGame game)
                            )

                _ ->
                    case model.game of
                        Just game ->
                            ( model
                            , Cmd.batch (sendGetGame game)
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

        CheckCard card ->
            case model.game of
                Just game ->
                    let
                        player =
                            getPlayer game.round model.userName
                    in
                    case player.exchange of
                        Just _ ->
                            ( setSelection model
                                game
                                (List.sortWith cardOrder <|
                                    case List.member card model.selection of
                                        True ->
                                            List.filter ((/=) card) model.selection

                                        False ->
                                            card :: model.selection
                                )
                            , Cmd.none
                            )

                        Nothing ->
                            ( setSelection model game [ card ]
                            , Cmd.none
                            )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CommandResult result ->
            ( model
            , Cmd.none
            )

        SeeAllCards ->
            sendCommand ctx model <|
                postCommand (model.name ++ "/seeAllCards") games

        DeclareGrandTichu ->
            sendCommand ctx model <|
                postCommand (model.name ++ "/declareGrandTichu") games

        DeclareTichu ->
            sendCommand ctx model <|
                postCommand (model.name ++ "/declareTichu") games

        MahJongRequest demand ->
            ( { model | demand = demand }
            , Cmd.none
            )

        ChoosePhoenixMeaning phoenixMeaning ->
            ( { model | phoenixMeaning = Just phoenixMeaning }
            , Cmd.none
            )

        SendHand result ->
            case model.game of
                Just game ->
                    games
                        |> withBody (encodeCards model.selection)
                        |> postCommand (model.name ++ "/hand")
                        |> sendCommand ctx
                            ({ model
                                | demand = Nothing
                                , game =
                                    Maybe.map
                                        (\game_ ->
                                            { game_
                                                | round =
                                                    modifyPlayer model.userName
                                                        (\player ->
                                                            { player | hand = removeCards model.selection player.hand }
                                                        )
                                                        game_.round
                                            }
                                        )
                                        model.game
                             }
                                |> clearSelection
                            )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        PlaceCombination ->
            case model.game of
                Just game ->
                    case not <| List.isEmpty model.possibilities of
                        True ->
                            case ( List.member MahJong model.selection, model.demand ) of
                                ( True, Just rank ) ->
                                    games
                                        |> withBody (JE.encode 0 (JE.object [ ( "rank", rankEncoder rank ) ]))
                                        |> postCommand (model.name ++ "/demandRank")
                                        |> sendCustomCommand SendHand
                                            ctx
                                            model

                                _ ->
                                    update ctx (SendHand (Ok "")) model

                        False ->
                            ( clearSelection { model | demand = Nothing }
                            , Cmd.none
                            )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        Pass ->
            sendCommand ctx model <|
                postCommand (model.name ++ "/pass") games

        Exchange ->
            Maybe.map3
                (\a b c ->
                    games
                        |> withBody (encodeCards [ a, b, c ])
                        |> postCommand (model.name ++ "/exchangeCards")
                        |> sendCommand ctx
                            { model
                                | selection = []
                                , possibilities = []
                                , exchange = Dict.empty
                            }
                )
                (Dict.get 0 model.exchange)
                (Dict.get 1 model.exchange)
                (Dict.get 2 model.exchange)
                |> Maybe.withDefault
                    ( model
                    , Cmd.none
                    )

        Start ->
            ( model
            , Cmd.batch
                (case model.awaitingTable of
                    Just t ->
                        [ startGame t.name <| ctx.mapMsg << SentStart ]

                    _ ->
                        []
                )
            )

        SentStart result ->
            ( model
            , getTable model.name 0 ctx
                |> Task.attempt UpdateTables
                |> Cmd.map ctx.mapMsg
            )

        SetExchange i ->
            ( exchangeCard model i
            , Cmd.none
            )

        PlayerClicked name ->
            case model.game of
                Just game ->
                    if isGivingDragon model game then
                        case isNextOpponent game.round model.userName name of
                            Just b ->
                                games
                                    |> withBody
                                        (if b then
                                            "next"

                                         else
                                            "previous"
                                        )
                                    |> postCommand (model.name ++ "/giveDragon")
                                    |> sendCommand ctx model

                            _ ->
                                ( model
                                , Cmd.none
                                )

                    else
                        ( model
                        , Cmd.none
                        )

                Nothing ->
                    ( model
                    , Cmd.none
                    )


isGivingDragon model game =
    (getActualPlayer game.round).name
        == model.userName
        && (case List.head game.round.table of
                Just [ Dragon ] ->
                    True

                _ ->
                    False
           )


clearSelection model =
    { model
        | selection = []
        , possibilities = []
        , phoenixMeaning = Nothing
    }


setSelection model game selection =
    { model
        | selection =
            List.sortWith cardOrder selection
        , possibilities =
            case List.member Phoenix selection of
                True ->
                    let
                        noMahjong =
                            removeCards [ Phoenix ] selection

                        versions =
                            List.range 0 (List.length noMahjong)
                    in
                    versions
                        |> List.map
                            (\i ->
                                let
                                    permutation =
                                        List.take i noMahjong
                                            ++ [ Phoenix ]
                                            ++ List.drop i noMahjong
                                in
                                permutation
                                    |> allowedCombination game.round.table
                                    |> (\isAllowed ->
                                            case isAllowed of
                                                True ->
                                                    parseTrick permutation

                                                False ->
                                                    Nothing
                                       )
                            )
                        |> List.filterMap identity

                False ->
                    case allowedCombination game.round.table selection of
                        True ->
                            [ parseTrick selection ]
                                |> List.filterMap identity

                        False ->
                            []
        , phoenixMeaning = Nothing
    }


exchangeCard model index =
    case model.selection of
        [ c ] ->
            Dict.filter (\k v -> v /= c) model.exchange
                |> Dict.insert index c
                |> (\d ->
                        { model
                            | exchange = d
                            , selection = []
                            , possibilities = []
                        }
                   )

        _ ->
            model


sendCommand ctx model task =
    ( model
    , task
        |> Task.attempt CommandResult
        |> Cmd.map ctx.mapMsg
    )


sendCustomCommand cmd ctx model task =
    ( model
    , task
        |> Task.attempt cmd
        |> Cmd.map ctx.mapMsg
    )



-- VIEW


orElse maybe generator =
    case maybe of
        Nothing ->
            generator

        _ ->
            maybe


view : ComponentView Model msg Msg
view ctx model =
    Page "Table" <|
        div [] <|
            [ node "style" [] [ text cssStyle ] ]
                ++ (case model.game of
                        Just game ->
                            [ gameView ctx model.userName model game ]

                        Nothing ->
                            case model.awaitingTable of
                                Just awaitingTable ->
                                    [ awaitingTableView ctx awaitingTable ]

                                Nothing ->
                                    []
                   )


playerBox className table index =
    div [ class className ]
        [ case List.drop index table.users |> List.head of
            Just player ->
                text player.name

            _ ->
                text "awaiting for player"
        ]


playerGameBox : List Card -> String -> Game -> Int -> Html Msg
playerGameBox selection className game index =
    case List.drop index game.round.players |> List.head of
        Just player ->
            div
                [ class className
                , onClick (PlayerClicked player.name)
                ]
            <|
                (case player.hand of
                    [] ->
                        printHiddenCards player

                    _ ->
                        printTableHand selection player.hand
                )
                    ++ [ case game.round.actualPlayer == index of
                            True ->
                                span [ class "actual-player" ] [ text player.name ]

                            False ->
                                case game.round.tableHandOwner of
                                    Just i ->
                                        if i == index then
                                            span [ class "table-hand-owner" ]
                                                [ text player.name ]

                                        else
                                            text player.name

                                    _ ->
                                        text player.name
                       ]

        _ ->
            div [ class className ] [ text "error" ]


chatPanel chatData =
    div [ class "col-md-2 hidden-md-down" ]
        [ div [ class "table-chat" ]
            [ div [ class "chat-header" ] [ text "Chat" ]
            , div [] [ text "fsdds" ]
            , div [] [ text "asdf" ]
            ]
        ]


gameSummaryPanel game =
    div [ class "col-lg-2 col-md-3 hidden-sm-down" ]
        [ div [ class "table-options" ]
            [ div [ class "table-options-header" ] [ text "Game" ]
            , div [] [ text <| Debug.toString <| calculateTeamPoints game.history ]
            , div []
                (calculateHistoryPoints game.history
                    |> List.map
                        (\v ->
                            div [] [ text <| Debug.toString v ]
                        )
                )
            ]
        , div [ class "table-chat hidden-lg-up hidden-sm-down" ]
            [ div [ class "chat-header" ] [ text "Chat" ]
            , div [] [ text "fsdds" ]
            , div [] [ text "asdf" ]
            ]
        ]


awaitingTableView : Context msg Msg -> AwaitingTable -> Html msg
awaitingTableView ctx table =
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ chatPanel ()
            , div [ class "col-md-8" ]
                [ div [ class "table-main" ]
                    [ playerBox "player-bottom" table 0
                    , playerBox "player-right" table 1
                    , playerBox "player-top" table 2
                    , playerBox "player-left" table 3
                    , List.head table.users
                        |> Maybe.map (.pressedStart >> not >> (&&) (List.length table.users == 4))
                        |> Maybe.andThen
                            (\show ->
                                case show of
                                    True ->
                                        Just <|
                                            button
                                                [ class "btn btn-primary middle-table"
                                                , type_ "button"
                                                , onClick <| ctx.mapMsg Start
                                                ]
                                                [ text "Start" ]

                                    _ ->
                                        Nothing
                            )
                        |> Maybe.withDefault (div [] [])
                    ]
                ]
            ]
        ]


gameButton condition msg title =
    case condition of
        True ->
            button
                [ class "btn btn-primary"
                , onClick msg
                ]
                [ text title ]
                |> Just

        False ->
            Nothing


grandTichuButton game player =
    gameButton
        (not player.sawAllCards
            && not player.grandTichu
        )
        DeclareGrandTichu
        "Grand Tichu"


seeAllCardsButton game player =
    gameButton
        (not player.sawAllCards)
        SeeAllCards
        "See All Cards"


tichuButton game player =
    gameButton
        (player.sawAllCards
            && player.cardsOnHand
            == 14
            && not player.tichu
        )
        DeclareTichu
        "Tichu"


playButton userName game player model =
    gameButton
        (player.sawAllCards
            && not (isNothing player.exchange)
            && (getActualPlayer game.round).name
            == userName
            && (not <| List.isEmpty model.possibilities)
        )
        PlaceCombination
        "Play"


passButton userName game player =
    gameButton
        (player.sawAllCards
            && not (isNothing player.exchange)
            && ((getActualPlayer game.round).name
                    == userName
                    && not (List.isEmpty game.round.table)
               )
        )
        Pass
        "Pass"


exchangeButton userName game player =
    gameButton
        (player.sawAllCards && isNothing player.exchange)
        Exchange
        "Exchange"


styleProperCombination model =
    case model.possibilities of
        h :: t ->
            "proper-combination"

        _ ->
            ""


gameView : Context msg Msg -> String -> Model -> Game -> Html msg
gameView ctx userName model game =
    List.filter (.name >> (==) userName) game.round.players
        |> List.head
        |> Maybe.map
            (\player ->
                div [ class "row" ]
                    [ chatPanel game
                    , div [ class "col-lg-8 col-md-9 col-sm-12" ] <|
                        [ Html.map ctx.mapMsg <|
                            div [ class ("table-main game-table " ++ styleProperCombination model) ]
                                [ div []
                                    [ div []
                                        [ div []
                                            [ playerGameBox model.selection "player-top" game 2 ]
                                        ]
                                    ]
                                , div []
                                    [ div []
                                        [ div [ class "game-table" ]
                                            [ div [ class "middle-table-row" ]
                                                [ div [ class "player-box" ]
                                                    [ playerGameBox model.selection "player-left" game 3 ]
                                                , div []
                                                    -- mahjong request panel
                                                    [ case ( List.member MahJong model.selection, player.exchange ) of
                                                        ( True, Just a ) ->
                                                            ( "N", Nothing )
                                                                :: List.map
                                                                    (\rank ->
                                                                        ( case rank of
                                                                            R i ->
                                                                                Debug.toString i

                                                                            a_ ->
                                                                                Debug.toString a_
                                                                        , Just rank
                                                                        )
                                                                    )
                                                                    allowedRanks
                                                                |> List.map
                                                                    (\( title, value ) ->
                                                                        div
                                                                            [ class
                                                                                ("btn btn-default"
                                                                                    ++ (if model.demand == value then
                                                                                            " active"

                                                                                        else
                                                                                            ""
                                                                                       )
                                                                                )
                                                                            , onClick (MahJongRequest value)
                                                                            ]
                                                                            [ text title ]
                                                                    )
                                                                |> div [ class "btn-group mahjong-demand" ]

                                                        _ ->
                                                            div [] []

                                                    -- choose combination, show combination
                                                    , case model.possibilities of
                                                        [] ->
                                                            div [] []

                                                        [ p ] ->
                                                            div [ class "float-left" ] [ text (Debug.toString p) ]

                                                        _ ->
                                                            div [ class "btn-group phoenix-meaning float-left" ] <|
                                                                List.map
                                                                    (\possibility ->
                                                                        div
                                                                            [ class
                                                                                ("btn btn-default"
                                                                                    ++ (model.phoenixMeaning
                                                                                            |> Maybe.map
                                                                                                (\meaning ->
                                                                                                    if Debug.toString possibility == meaning then
                                                                                                        " active"

                                                                                                    else
                                                                                                        ""
                                                                                                )
                                                                                            |> Maybe.withDefault ""
                                                                                       )
                                                                                )
                                                                            , onClick
                                                                                (ChoosePhoenixMeaning
                                                                                    (Debug.toString possibility)
                                                                                )
                                                                            ]
                                                                            [ text (Debug.toString possibility) ]
                                                                    )
                                                                    model.possibilities

                                                    -- give dragon info
                                                    , if isGivingDragon model game then
                                                        div [ class "float-left" ] [ text "Give the Dragon" ]

                                                      else
                                                        div [] []

                                                    -- open demand info
                                                    , case ( game.round.demand, game.round.demandCompleted ) of
                                                        ( Just r, False ) ->
                                                            div [ class "float-left" ] [ text ("Open demand for " ++ Debug.toString r) ]

                                                        _ ->
                                                            div [] []

                                                    -- exchange panel
                                                    , case player.exchange of
                                                        Just _ ->
                                                            case game.round.table of
                                                                h :: t ->
                                                                    div [ class "cards-on-table " ] (printCards h [])

                                                                _ ->
                                                                    div [] []

                                                        Nothing ->
                                                            if player.sawAllCards then
                                                                div [ class "card-exchange" ]
                                                                    [ div [ onClick (SetExchange 0) ]
                                                                        [ div [] (printExchangeCard model 0) ]
                                                                    , div [ onClick (SetExchange 1) ]
                                                                        [ div [] (printExchangeCard model 1) ]
                                                                    , div [ onClick (SetExchange 2) ]
                                                                        [ div [] (printExchangeCard model 2) ]
                                                                    ]

                                                            else
                                                                div [] []
                                                    ]
                                                , div [ class "player-box" ]
                                                    [ playerGameBox model.selection "player-right" game 1 ]
                                                ]
                                            ]
                                        ]
                                    ]
                                , div []
                                    [ div []
                                        [ playerGameBox model.selection "player-bottom" game 0 ]
                                    ]
                                , div []
                                    [ div [ class "" ]
                                        -- game buttons
                                        [ div [ class "game-buttons" ] <|
                                            List.filterMap identity
                                                [ grandTichuButton game player
                                                , seeAllCardsButton game player
                                                , tichuButton game player
                                                , playButton userName game player model
                                                , passButton userName game player
                                                , exchangeButton userName game player
                                                ]
                                        ]
                                    ]
                                ]
                        ]
                    , gameSummaryPanel game
                    ]
            )
        |> Maybe.withDefault (div [] [ text <| "Could not find player " ++ userName ])
        |> List.singleton
        |> div [ class "container-fluid" ]


printExchangeCard model index =
    [ case Dict.get index model.exchange of
        Just c ->
            printCardSkeleton c

        _ ->
            text ""
    ]


oldTichuView : List Card -> Game -> Html Msg
oldTichuView selection game =
    div []
        --[ node "link" [ rel "stylesheet", href "https://bootswatch.com/darkly/bootstrap.css" ] []
        [ node "style" [] [ text cssStyle ]
        , div [ class "" ]
            (List.map printRow
                [ [ showRound selection game.round ]
                , [ button
                        [ class "btn btn-sm btn-primary"
                        , onClick PlaceCombination
                        ]
                        [ text "Place" ]
                  ]

                --        , h3 [] [ text "Logs" ] :: map (\l -> div [ class "text-muted" ] [ text (Debug.toString l) ]) game.log
                ]
            )
        ]


printRow : List (Html Msg) -> Html Msg
printRow content =
    div [ class "row" ] [ div [ class "col-md-12" ] content ]


printCards : List Card -> List Card -> List (Html Msg)
printCards cards selection =
    List.map (printCard selection) (List.sortWith cardOrder cards)


printCard : List Card -> Card -> Html Msg
printCard selection card =
    div
        [ class <|
            String.join " "
                [ "card-outer"
                , "selected-" ++ Debug.toString (List.member card selection)
                ]
        , onClick (CheckCard card)
        ]
        [ printCardSkeleton card ]


printCardSkeleton : Card -> Html Msg
printCardSkeleton card =
    case card of
        NormalCard suit rank ->
            div [ class <| "suit-mark " ++ (Debug.toString suit |> String.toLower) ]
                [ text <|
                    case rank of
                        R i ->
                            Debug.toString i

                        r ->
                            Debug.toString r
                ]

        -- special cards
        a ->
            text (Debug.toString a)


printTableHand : List Card -> List Card -> List (Html Msg)
printTableHand selection cards =
    printCards (List.sortWith cardOrder cards) selection


printHiddenCards : Player -> List (Html Msg)
printHiddenCards player =
    [ div [ class "card-outer" ]
        [ text
            (Debug.toString <|
                if player.sawAllCards then
                    player.cardsOnHand

                else
                    8
            )
        ]
    ]


showRound : List Card -> Round -> Html Msg
showRound selection round =
    let
        table =
            List.map (\s -> div [] s)
                (List.map (printTableHand selection) round.table)

        players =
            List.map (showPlayer selection round.actualPlayer)
                (List.indexedMap (\a b -> ( a, b )) round.players)
    in
    div [] (table ++ players)


showPlayer : List Card -> Int -> ( Int, Player ) -> Html Msg
showPlayer selection actualPlayer ( index, player ) =
    div [] <|
        div
            [ class
                (""
                    ++ (if actualPlayer == index then
                            "text-success"

                        else
                            ""
                       )
                )
            ]
            [ text (player.name ++ " " ++ Debug.toString player.cardsOnHand) ]
            :: printCards player.hand selection
