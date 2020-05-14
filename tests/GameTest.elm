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
                Expect.equal 21 <| score [ Card Spades 10, Card Hearts 1 ]
        , test "normal score" <|
            \_ ->
                Expect.equal 17 <| score [ Card Clubs 7, Card Diamonds 12 ]
        , test "soft score" <|
            \_ ->
                Expect.equal 20 <| score [ Card Spades 5, Card Diamonds 4, Card Clubs 10, Card Hearts 1 ]
        , test "royals round down" <|
            \_ ->
                Expect.equal 20 <| score [ Card Spades 10, Card Hearts 12 ]
        , test "busted score" <|
            \_ ->
                Expect.equal 23 <| score [ Card Spades 10, Card Hearts 12, Card Diamonds 3 ]
        ]
