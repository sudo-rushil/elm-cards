module Cards exposing (..)

{-|


# Construction

@docs Suit, Card

-}

import Html exposing (Html, text)
import Html.Attributes exposing (class, style)


checkTrue : String -> Bool
checkTrue _ =
    True



-- CARDS


type Suit
    = Spades
    | Diamonds
    | Clubs
    | Hearts


type Card
    = Card Suit Int
    | Back


viewCard : Card -> ( String, String )
viewCard card =
    case card of
        Card Spades value ->
            ( "black", viewFace 0x0001F0A0 value )

        Card Diamonds value ->
            ( "red", viewFace 0x0001F0B0 value )

        Card Clubs value ->
            ( "black", viewFace 0x0001F0C0 value )

        Card Hearts value ->
            ( "red", viewFace 0x0001F0D0 value )

        Back ->
            ( "black", viewFace 0x0001F0A0 0 )


viewFace : Int -> Int -> String
viewFace suit face =
    Char.fromCode (suit + face)
        |> String.fromChar
