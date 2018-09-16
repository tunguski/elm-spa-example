module Tichu exposing (main, subscriptions)

{-| <https://en.wikipedia.org/wiki/Tichu>
-}

import Tichu.Model exposing (..)
import Tichu.Update exposing (..)
import Tichu.View exposing (..)



-----------------------------------------------------------------------------
-- MAIN
-----------------------------------------------------------------------------
-- wire the entire application together


main =
    App.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--Signal.map (view actions.address) model
{-
   shuffle : List a -> List a
   shuffle lt =
     let len = length lt
         fgen = Random.float 0 1
         lgen = Random.list len fgen
         rlist = fst <| Random.generate lgen (Random.initialSeed 31415)
         total = sum rlist
         nlist = map (\x -> x / total) rlist
         zip l1 l2 =
           case l1 of
             [] -> []
             x :: xs ->
               case l2 of
                 [] -> []
                 y :: ys -> (x,y) :: zip xs ys
         tlist = zip lt nlist
         flist = sortBy snd tlist
     in fst <| unzip flist
-}
-----------------------------------------------------------------------------
-- SUBSCRIPTIONS
-----------------------------------------------------------------------------


subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.none
