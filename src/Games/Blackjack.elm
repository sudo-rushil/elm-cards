module Games.Blackjack exposing (checkTrue)

{-|

@docs checkTrue

-}

import Cards exposing (Card(..), Suit(..))
import Deck exposing (Deck)


{-| Hi
-}
checkTrue : String -> Bool
checkTrue _ =
    True


cardValue : Card -> Int
cardValue card =
    case card of
        Card _ face ->
            if face > 10 then
                10

            else if face == 1 then
                11

            else
                face

        _ ->
            0


score : List Card -> Int
score hand =
    let
        raw : List Int
        raw =
            List.map cardValue hand

        rawScore : Int
        rawScore =
            List.sum raw
    in
    if rawScore > 21 && List.member 11 raw then
        rawScore - 10

    else
        rawScore
