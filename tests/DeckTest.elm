module DeckTest exposing (..)

import Deck exposing (..)
import Expect
import Test exposing (Test, describe, test)



-- Test Suite


checkSuite : Test
checkSuite =
    describe "check project compiles"
        [ test "check function returns true" <|
            \_ ->
                Expect.true "hello" True
        ]
