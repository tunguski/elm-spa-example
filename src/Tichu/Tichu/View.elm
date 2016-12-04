module Tichu.View exposing (..)


import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)

import Tichu.Model exposing (..)
import Tichu.Update exposing (..)

import List exposing (..)
import String
import Dict exposing (Dict)

-----------------------------------------------------------------------------
-- VIEW
-----------------------------------------------------------------------------


view : Game -> Html Msg
view game =
  div [] [
    node "link" [ rel "stylesheet", href "https://bootswatch.com/darkly/bootstrap.css" ] [],
    node "style" [] [ text (cssStyle game) ],
    div [ class "container" ] 
      (map printRow [
        [ h1 [] [ text "Test" ] ]
        , [ showRound game.round ]
        , [ button [
            class "btn btn-sm btn-primary",
            onClick PlaceCombination
          ] [ text "Place" ] ]
        , [ button [
            class "btn btn-sm btn-info",
            onClick MorePlease 
          ] [ text "More Please" ] ]
--        , h3 [] [ text "Logs" ] :: map (\l -> div [ class "text-muted" ] [ text (toString l) ]) game.log
      ])
  ]
  
printRow : List (Html Msg) -> Html Msg
printRow content =
      div [ class "row" ] [ div [ class "col-md-12" ] content ]
  
  
printCards : List Card -> List Card -> List (Html Msg)
printCards cards selection =
  map (printCard selection) cards
  

printCard : List Card -> Card -> Html Msg
printCard selection card =
  div [ class <| String.join " " [
      "card-outer"
    , "selected-" ++ (toString (member card selection))
    ], onClick (CheckCard card) ] 
    [ printCardSkeleton card ]


printCardSkeleton : Card -> Html Msg
printCardSkeleton card =
  case card of
    NormalCard suit rank ->
      div [ class <| "suit-mark " ++ (toString suit |> String.toLower) ] [
        text <| case rank of
                  R i -> toString i
                  r -> toString r
      ]
    -- special cards
    a -> text (toString a)
    

printTableHand : List Card -> List (Html Msg)
printTableHand cards =
  map (printCard []) cards

showRound : Round -> Html Msg
showRound round =
  let table = map (\s -> div [] s) (map (printTableHand) round.table)
      players = map (showPlayer round.actualPlayer) (Dict.toList round.players)
  in
    div [] ([ h2 [] [ text ("Round") ]
            , h3 [] [ text ("Table") ]
            ]
         ++ table
         ++ [ h3 [] [ text ("Players") ] ]
         ++ players
         )
  

showPlayer : Int -> (Int, Player) -> Html Msg
showPlayer actualPlayer (index, player) =
  div [] <| (div [ class ("" ++ (if actualPlayer == index then "text-success" else "")) ] 
                 [ text player.name ]) 
            :: printCards player.hand player.selection


-----------------------------------------------------------------------------
-- CSS STYLES
-----------------------------------------------------------------------------

cssStyle : Game -> String
cssStyle game = """
.card-outer {
  border: 1px solid grey;
  border-radius: 7px;
  display: inline-block;
  margin: 5px;
  padding: 3px 5px;
}
.card-outer:hover {
  border-color: blue;
  background-color: #777;
  cursor: pointer;
}
.selected-True {
  background-color: #555;
}
.suit-mark {
}
.suit-mark:after {
  margin-left: 0.3em;
}
.spades:after {
  content: '♠';
}
.hearts:after {
  content: '♥';
  color: green;
}
.diamonds:after {
  content: '♦';
  color: red;
}
.clubs:after {
  content: '♣';
  color: blue;
}
"""


