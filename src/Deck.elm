module Deck exposing
    ( Deck
    , fullSuit, fullFace, fullDeck, randomDeck
    , appendCard
    )

{-| Deck types, generators, and manipulating functions


# Types

@docs Deck


# Construction

@docs fullSuit, fullFace, fullDeck, randomDeck


# Manipulation

@docs appendCard

-}

import Cards exposing (Card(..), Suit(..))
import Random
import Random.List exposing (shuffle)


{-| A representation of an arbitrary deck or hand of cards.
-}
type alias Deck =
    List Card


{-| Make a deck of all the cards in a single suit.

Makes the deck in A-K order

    fullSuit Spades == [ Card Spades 1, Card Spades 2, Card Spades 3, Card Spades 4, Card Spades 5, Card Spades 6, Card Spades 7, Card Spades 8, Card Spades 9, Card Spades 10, Card Spades 11, Card Spades 12, Card Spades 13 ]

-}
fullSuit : Suit -> Deck
fullSuit suit =
    List.map (Card suit) <| List.range 1 13


{-| Make a deck of all the cards for a single face.

Makes the deck in standard order.

    fullFace 1 == [ Card Spades 1, Card Diamonds 1, Card Clubs 1, Card Hearts 1 ]

-}
fullFace : Int -> Deck
fullFace face =
    List.map (\f -> Card f face) <| [ Spades, Diamonds, Clubs, Hearts ]


{-| A full 52-card deck in standard order.

    fullDeck == [ Card Spades 1, Card Spades 2, ... ]

-}
fullDeck : Deck
fullDeck =
    let
        suits : List Suit
        suits =
            [ Spades, Diamonds, Clubs, Hearts ]
    in
    List.map fullSuit suits
        |> List.concat


{-| A 52-card deck in randomly shuffled order.

    type Msg = ShuffleDeck Deck
    Random.generate ShuffleDeck randomDeck

-}
randomDeck : Random.Generator Deck
randomDeck =
    shuffle fullDeck


{-| Add a card to the end of the deck.

    appendCard (Card Spades 1) [ Card Spades 3, Card Spades 2 ] == [ Card Spades 3, Card Spades 2, Card Spades 1 ]

-}
appendCard : Card -> Deck -> Deck
appendCard card deck =
    List.foldr (::) [ card ] deck
