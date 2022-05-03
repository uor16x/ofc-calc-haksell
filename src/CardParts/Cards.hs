-- | Combined (value + suit) card data type and its processing methods
module CardParts.Cards where

import CardParts.Suits ( Suit (..), parseSuit )
import CardParts.Values ( Value (..), parseValue )

-- | This type represents a card - combination of suit and value.
data Card = Card {
    value :: Value,
    suit :: Suit
} deriving (Eq, Show)

{- | This method gets a string which represents full card combination (value + suit)
and returns a 'Card' wrapped with 'Maybe'. The function uses 'parseValue' and 'parseSuit' methods.

Passed string should have length 2 and have format "{VALUE}{SUIT}".

__Examples:__

@
parseCard "2c" = 'Just' 'Two' 'Clubs'
parseCard "As" = 'Just' 'Ace' 'Spades'
parseCard "22c" = 'Nothing'
parseCard "5f" = 'Nothing'
parseCard "2x" = 'Nothing'
@
-}
parseCard :: String -> Maybe Card
parseCard [] = Nothing
parseCard str
    | length str /= 2 = Nothing
    | otherwise = do
        value <- parseValue $ head str
        suit <- parseSuit $ last str
        return $ Card { value = value, suit = suit }
