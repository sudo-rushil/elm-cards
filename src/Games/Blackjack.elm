module Games.Blackjack exposing (score)

{-| Scoring for Blackjack


# Scoring

@docs score

-}

import Cards exposing (Card(..), Suit(..))
import Deck exposing (Deck)


{-| Hi
-}
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


{-| Score the input hand.

Score calculated according to standard blackjack rules. If a hand has an ace that is, when scored as an eleven, causes the hand to exceed 21, the ace is acounted as a one.

    score [ Card Spades 10, Card Hearts 1 ] == 21

    score [ Card Clubs 5, Card Diamonds 12 ] == 17

    score [ Card Spades 5, Card Diamonds 4, Card Clubs 10, Card Hearts 1 ] == 20

-}
score : Deck -> Int
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
