module DeckTest exposing (..)

import Cards exposing (..)
import Deck exposing (..)
import Expect
import Test exposing (Test, describe, test)


testDeck : ShuffledDeck
testDeck =
    newDeck [ Card Spades Ace, Card Spades Two, Card Spades Three ]


testFn : Card -> Int
testFn card =
    case card of
        Card _ face ->
            case face of
                Ace ->
                    1

                Two ->
                    2

                Three ->
                    3

                Four ->
                    4

                Five ->
                    5

                Six ->
                    6

                Seven ->
                    7

                Eight ->
                    8

                Nine ->
                    9

                Ten ->
                    10

                Jack ->
                    11

                Queen ->
                    12

                King ->
                    13

        Back ->
            0


testFoldFn : Card -> Int -> Int
testFoldFn card int =
    int + testFn card



-- Test Suite


constructionSuite : Test
constructionSuite =
    describe "check making a deck works as expected"
        [ test "check fullSuit" <|
            \_ ->
                Expect.equal (fullSuit Spades) [ Card Spades Ace, Card Spades Two, Card Spades Three, Card Spades Four, Card Spades Five, Card Spades Six, Card Spades Seven, Card Spades Eight, Card Spades Nine, Card Spades Ten, Card Spades Jack, Card Spades Queen, Card Spades King ]
        , test "check fullFace" <|
            \_ ->
                Expect.equal (fullFace Ace) [ Card Spades Ace, Card Diamonds Ace, Card Clubs Ace, Card Hearts Ace ]
        , test "check newDeck and fullDeck" <|
            \_ ->
                Expect.equal (Deck.take 3 fullDeck) testDeck
        ]


manipulationSuite : Test
manipulationSuite =
    describe "check manipulating a deck works as expected"
        [ test "check appendCard" <|
            \_ ->
                Expect.equal (appendCard (Card Spades Four) testDeck) (newDeck [ Card Spades Ace, Card Spades Two, Card Spades Three, Card Spades Four ])
        , test "check length" <|
            \_ ->
                Expect.equal (length testDeck) 3
        , test "check draw" <|
            \_ ->
                case draw testDeck of
                    ( card, _ ) ->
                        Expect.equal card (Card Spades Ace)
        , test "check map" <|
            \_ ->
                Expect.equal (Deck.map testFn testDeck) [ 1, 2, 3 ]
        , test "check foldr" <|
            \_ ->
                Expect.equal (Deck.foldr testFoldFn 0 testDeck) 6
        , test "check foldl" <|
            \_ ->
                Expect.equal (Deck.foldl testFoldFn 0 testDeck) 6
        , test "take" <|
            \_ ->
                Expect.equal (Deck.take 2 testDeck) (newDeck [ Card Spades Ace, Card Spades Two ])
        ]
