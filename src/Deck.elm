module Deck exposing (Deck)

{-| Deck types, generators, and manipulating functions


# Types

@docs Deck


# Construction


# Manipulation

-}

import Cards exposing (Card(..), Suit(..))
import Random
import Random.List exposing (shuffle)


{-| A representation of an arbitrary deck or hand of cards
-}
type alias Deck =
    List Card


checkTrue : String -> Bool
checkTrue _ =
    True


initSuit : Suit -> List Card
initSuit suit =
    List.map (Card suit) <| List.range 1 13


initDeck : Random.Generator Deck
initDeck =
    let
        suits : List Suit
        suits =
            [ Spades, Diamonds, Clubs, Hearts ]
    in
    List.map initSuit suits
        |> List.concat
        |> shuffle


cardToEnd : Deck -> Card -> Deck
cardToEnd deck card =
    List.foldr (::) [ card ] deck
