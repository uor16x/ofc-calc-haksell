-- | Combined (value + suit) card data type and its processing methods
module CardParts.Cards where

import CardParts.Suits ( Suit (..), parseSuit )
import CardParts.Values ( Value (..), parseValue )

-- | This type represents a card - combination of suit and value.
data Card = Card {
    value :: Value,
    suit :: Suit
} deriving (Show)

instance Eq Card where
    c1@Card {} == c2@Card {} = value c1 == value c2

instance Ord Card where
    c1@Card {} `compare` c2@Card {} = value c1 `compare` value c2

{- | This method gets a string which represents full card combination (value + suit)
and returns a 'Card' wrapped with 'Right'. The function uses 'parseValue' and 'parseSuit' methods.

Passed string should have length 2 and have format "{VALUE}{SUIT}".

__Examples:__

@
parseCard "2c" = 'Right' 'Two' 'Clubs'
parseCard \"As\" = 'Right' 'Ace' 'Spades'
parseCard "" = 'Left' "Can't process emtpy string"
parseCard "22c" = 'Left' "Argument length should be 2"
parseCard "Zd" = 'Left' "There is no broadway card, which could be represented with \'Z\'"
parseCard "5f" = 'Left' "There is no card suit marked as \'f\'"
parseCard "2x" = 'Left' "There is no card suit marked as \'x\'"
@
-}
parseCard :: String -> Either String Card
parseCard [] = Left "Can't process emtpy string"
parseCard str
    | length str /= 2 = Left "Argument length should be 2"
    | otherwise = do
        value <- parseValue $ head str
        suit <- parseSuit $ last str
        return $ Card { value = value, suit = suit }

