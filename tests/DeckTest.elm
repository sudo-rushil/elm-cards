module DeckTest exposing (..)

import Expect
import Games.Blackjack exposing (..)
import Test exposing (Test, describe, test)



-- Test Suite


checkSuite : Test
checkSuite =
    describe "check project compiles"
        [ test "check function returns true" <|
            \_ ->
                Expect.true "hello" <| checkTrue "hello"
        ]
