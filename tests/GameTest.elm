module GameTest exposing (..)

import Cards exposing (..)
import Expect
import Games.Blackjack exposing (..)
import Test exposing (Test, describe, test)



-- Test Suite


blackjackSuite : Test
blackjackSuite =
    describe "check Blackjack scoring is correct"
        [ test "blackjack score" <|
            \_ ->
                Expect.equal 21 <| score [ Card Spades Ten, Card Hearts Ace ]
        , test "normal score" <|
            \_ ->
                Expect.equal 17 <| score [ Card Clubs Seven, Card Diamonds Queen ]
        , test "soft score" <|
            \_ ->
                Expect.equal 20 <| score [ Card Spades Five, Card Diamonds Four, Card Clubs Ten, Card Hearts Ace ]
        , test "royals round down" <|
            \_ ->
                Expect.equal 20 <| score [ Card Spades Ten, Card Hearts Queen ]
        , test "busted score" <|
            \_ ->
                Expect.equal 23 <| score [ Card Spades Ten, Card Hearts Queen, Card Diamonds Three ]
        ]
