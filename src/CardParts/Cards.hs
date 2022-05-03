-- | Combined (value + suit) card data type and its processing methods
module CardParts.Cards where

import CardParts.Suits ( Suit, parseSuit )
import CardParts.Values ( Value, parseValue )

-- | This type represents a card - combination of suit and value.
data Card = Card {
    value :: Value,
    suit :: Suit
}
