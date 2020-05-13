module CardTest exposing (..)

import Cards exposing (..)
import Expect
import Test exposing (Test, describe, test)



-- Test Suite


cardSuite : Test
cardSuite =
    describe "check we can create cards"
        [ test "construct ace of spades" <|
            \_ ->
                case new "spades" 1 of
                    Just card ->
                        Expect.equal (Card Spades 1) card

                    Nothing ->
                        Expect.fail "Cards.new failed"
        , test "invalid card fails" <|
            \_ ->
                case new "horses" 1 of
                    Just card ->
                        Expect.fail "Invalid card created"

                    Nothing ->
                        Expect.pass
        , test "construct ace of spades with default" <|
            \_ ->
                Expect.equal (Card Spades 1) <| defaultNew Back "spades" 1
        , test "invalid card returns default" <|
            \_ ->
                Expect.equal Back <| defaultNew Back "horses" 1
        ]
