module Cards exposing
    ( Suit(..), Face(..), Card(..)
    , new, defaultNew
    , viewCard, defaultFace
    )

{-| Card datatypes and views

Use these for defining card-specifc game logic or for displaying specific cards.


# Types

@docs Suit, Face, Card


# Construction

@docs new, defaultNew


# Views

@docs viewCard, defaultFace

-}


{-| A playing card suit type.

Useful for pattern matching behavior of different games that are based on the suit of the card.

-}
type Suit
    = Spades
    | Diamonds
    | Clubs
    | Hearts


{-| A playing card face type.

Like the suit type above, this is useful for matching on different card faces, and does not induce an implicit order.

-}
type Face
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


resolveFace : Int -> Maybe Face
resolveFace face =
    case face of
        1 ->
            Just Ace

        2 ->
            Just Two

        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        6 ->
            Just Six

        7 ->
            Just Seven

        8 ->
            Just Eight

        9 ->
            Just Nine

        10 ->
            Just Ten

        11 ->
            Just Jack

        12 ->
            Just Queen

        13 ->
            Just King

        _ ->
            Nothing


{-| Default resoltuion of Faces to integers, in A-K order.

    defaultFace Ace == 1

    defaultFace King == 13

-}
defaultFace : Face -> Int
defaultFace face =
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


{-| A playing card type.

Can either hold a card of suit and face, or a blank card.

Face numbers are designated from 1 to 13 for A-K. Games which treat the ace differently can specify their behavior through the game logic.

The blank variant is useful for displaying cards that have not been flipped over, for instance.

    aceOfSpades =
        Card Spades Ace

    blankCard =
        Back

-}
type Card
    = Card Suit Face
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

    new "spades" 1 == Just (Card Spades Ace)

    new "SPADES" 1 == Just (Card Spades Ace)

    new "horses" 1 == Nothing

    new "spades" 0 == Nothing

-}
new : String -> Int -> Maybe Card
new suit face =
    if face < 1 || face > 13 then
        Nothing

    else
        case resolveSuit suit of
            Just cardSuit ->
                case resolveFace face of
                    Just cardFace ->
                        Just <| Card cardSuit cardFace

                    Nothing ->
                        Nothing

            Nothing ->
                Nothing


{-| Construct a new card with a default argument.

The first input is the default card to use if construction fails.

The remaining two inputs correspond to the two inputs for [Cards.new](Cards#new).

    defaultNew Back "spades" 1 == Card Spades Ace

    defaultNew Back "SPADES" 1 == Card Spades Ace

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
            ( "black", String.fromChar <| Char.fromCode 0x0001F0A0 )


viewFace : Int -> Face -> String
viewFace suit face =
    let
        faceVal =
            defaultFace face
    in
    Char.fromCode (suit + faceVal)
        |> String.fromChar
