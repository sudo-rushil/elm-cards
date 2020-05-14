module Cards exposing
    ( Suit(..), Card(..)
    , new, defaultNew
    , viewCard
    )

{-| Card datatypes and views

Use these for defining card-specifc game logic or for displaying specific cards.


# Types

@docs Suit, Card


# Construction

@docs new, defaultNew


# Views

@docs viewCard

-}


{-| A playing card suit type.

Useful for pattern matching behavior of different games that are based on the suit of the card.

-}
type Suit
    = Spades
    | Diamonds
    | Clubs
    | Hearts


{-| A playing card type.

Can either hold a card of suit and face, or a blank card.

Face numbers are designated from 1 to 13 for A-K. Games which treat the ace differently can specify their behavior through the game logic.

The blank variant is useful for displaying cards that have not been flipped over, for instance.

    aceOfSpades =
        Card Spades 1

    blankCard =
        Back

-}
type Card
    = Card Suit Int
    | Back


resolveSuit : String -> Maybe Suit
resolveSuit suit =
    case String.toLower suit of
        "spades" ->
            Just Spades

        "diamonds" ->
            Just Diamonds

        "clubs" ->
            Just Clubs

        "hearts" ->
            Just Hearts

        _ ->
            Nothing


{-| Construct a new card.

The first argument must be one of "spades", "diamonds", "clubs", or "hearts" (any case) for the card suit.

The second argument must be an integer from 1 to 13 for A-K.

Use [Cards.defaultNew](Cards#defaultNew) if you want a `Card` instead of a `Maybe Card`.

    new "spades" 1 == Just (Card Spades 1)

    new "SPADES" 1 == Just (Card Spades 1)

    new "horses" 1 == Nothing

    new "spades" 0 == Nothing

-}
new : String -> Int -> Maybe Card
new suit face =
    if face < 1 || face > 13 then
        Nothing

    else
        case resolveSuit suit of
            Just card ->
                Just <| Card card face

            Nothing ->
                Nothing


{-| Construct a new card with a default argument.

The first input is the default card to use if construction fails.

The remaining two inputs correspond to the two inputs for [Cards.new](Cards#new).

    defaultNew Back "spades" 1 == Card Spades 1

    defaultNew Back "SPADES" 1 == Card Spades 1

    defaultNew Back "horses" 1 == Back

    defaultNew Back "spades" 0 == Back

-}
defaultNew : Card -> String -> Int -> Card
defaultNew default suit face =
    Maybe.withDefault default <| new suit face


{-| Return the color and unicode string for a `Card`.

Use this function to write Html views for cards or decks.

    viewCard (defaultNew Back "spades" 1) == ( "black", "🂡" )

    viewCard (defaultNew Back "hearts" 7) == ( "red", "🃗" )

-}
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
