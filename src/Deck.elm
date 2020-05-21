module Deck exposing
    ( Deck, ShuffledDeck(..)
    , fullSuit, fullFace, fullDeck, newDeck, randomDeck
    , draw, appendCard, getCards, map, filter, foldr, foldl, take
    )

{-| Deck types, generators, and manipulating functions


# Types

@docs Deck, ShuffledDeck


# Construction

@docs fullSuit, fullFace, fullDeck, newDeck, randomDeck


# Manipulation

@docs draw, appendCard, getCards, map, filter, foldr, foldl, take

-}

import Cards exposing (Card(..), Face(..), Suit(..), defaultFace)
import Random
import Random.List exposing (shuffle)


{-| A representation of an arbitrary deck or hand of cards.
-}
type Deck
    = Deck (List Card)


{-| An exposed type for decks which are already shuffled.
-}
type ShuffledDeck
    = ShuffledDeck Deck


{-| Make a deck of all the cards in a single suit.

Makes the deck in A-K order

    fullSuit Spades == [ Card Spades Ace, Card Spades Two, Card Spades Three, Card Spades Four, Card Spades Five, Card Spades Six, Card Spades Seven, Card Spades Eight, Card Spades Nine, Card Spades Ten, Card Spades Jack, Card Spades Queen, Card Spades King ]

-}
fullSuit : Suit -> List Card
fullSuit suit =
    List.map (Card suit) [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]


{-| Make a deck of all the cards for a single face.

Makes the deck in standard order.

    fullFace Ace == [ Card Spades Ace, Card Diamonds Ace, Card Clubs Ace, Card Hearts Ace ]

-}
fullFace : Face -> List Card
fullFace face =
    List.map (\f -> Card f face) <| [ Spades, Diamonds, Clubs, Hearts ]


{-| Create a ShuffledDeck out of a list of cards.
-}
newDeck : List Card -> ShuffledDeck
newDeck cardList =
    ShuffledDeck <| Deck cardList


{-| A full 52-card deck in standard order.

    fullDeck == Deck [ Card Spades Ace, Card Spades Two, ... ]

-}
fullDeck : ShuffledDeck
fullDeck =
    let
        suits : List Suit
        suits =
            [ Spades, Diamonds, Clubs, Hearts ]
    in
    List.map fullSuit suits
        |> List.concat
        |> Deck
        |> ShuffledDeck


{-| A 52-card deck in randomly shuffled order.

    type Msg = ShuffleDeck Deck
    Random.generate ShuffleDeck randomDeck

-}
randomDeck : Random.Generator ShuffledDeck
randomDeck =
    case fullDeck of
        ShuffledDeck (Deck deck) ->
            Random.map (ShuffledDeck << Deck) <| shuffle deck


{-| Add a card to the end of the deck.

    appendCard (Card Spades Ace) [ Card Spades Three, Card Spades Two ] == [ Card Spades Three, Card Spades Two, Card Spades Ace ]

-}
appendCard : Card -> ShuffledDeck -> ShuffledDeck
appendCard card (ShuffledDeck (Deck cards)) =
    ShuffledDeck << Deck <| List.foldr (::) [ card ] cards


{-| Draw a card from a ShuffledDeck.

If the deck is empty, the card returned is `Back`.

    topCard =
        case draw (ShuffledDeck fullDeck) of
            ( top, _ ) ->
                top
    topCard == Card.new "spades" 1

-}
draw : ShuffledDeck -> ( Card, ShuffledDeck )
draw (ShuffledDeck deck) =
    case deck of
        Deck (x :: xs) ->
            ( x, ShuffledDeck (Deck xs) )

        -- Reached only if input deck is empty
        Deck [] ->
            ( Back, ShuffledDeck deck )


{-| Get the internal list of cards from a ShuffledDeck

Not reccomended for direct use.

-}
getCards : ShuffledDeck -> List Card
getCards (ShuffledDeck deck) =
    case deck of
        Deck cards ->
            cards


{-| Map a function from cards over a ShuffledDeck.

See <Games.Blackjack> for an example use of `Deck.map`

    map (\card -> 1) <| newDeck [ Card Spades Ace, Card Heart King ] == [ 1, 1 ]

    map defaultFace <| newDeck [ Card Spades Ace, Card Heart King ] == [ 1, 13 ]

-}
map : (Card -> a) -> ShuffledDeck -> List a
map f (ShuffledDeck deck) =
    case deck of
        Deck cards ->
            List.map f cards


{-| Fold a function over a ShuffledDeck from the right.
-}
foldr : (Card -> a -> a) -> a -> ShuffledDeck -> a
foldr f acc (ShuffledDeck deck) =
    case deck of
        Deck cards ->
            List.foldr f acc cards


{-| Fold a function over a ShuffledDeck from the left.
-}
foldl : (Card -> a -> a) -> a -> ShuffledDeck -> a
foldl f acc (ShuffledDeck deck) =
    case deck of
        Deck cards ->
            List.foldl f acc cards


{-| Take some number of cards from the top of a ShuffledDeck
-}
take : Int -> ShuffledDeck -> ShuffledDeck
take int (ShuffledDeck (Deck cards)) =
    ShuffledDeck << Deck <| List.take int cards


{-| Filter a ShuffledDeck by a conditional over cards.
-}
filter : (Card -> Bool) -> ShuffledDeck -> ShuffledDeck
filter p (ShuffledDeck (Deck deck)) =
    ShuffledDeck << Deck <| List.filter p deck
