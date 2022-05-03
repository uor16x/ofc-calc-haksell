module OfcCalc (
    Value(..),
    Suit(..),
    parseValue,
    parseSuit,
    Card
) where

import Data.List (elemIndex)
import Data.Char ( toLower, isDigit, digitToInt )

-- | This type represents card value.
data Value = Two
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
    | Ace deriving (Show, Eq, Enum, Ord, Bounded)

{- | This method gets a char which represents card value
and returns a 'Value' wrapped with 'Maybe'.

Char should be a digit between 2 and 9 or an uppercase symbol from [AKQJT] list.
Otherwise - 'Nothing' returns.

__Examples:__

@
parseValue \'2\' = 'Just' 'Two'
parseValue \'Q\' = 'Just' 'Queen'
parseValue \'z\' = 'Nothing'
@
-}
parseValue :: Char -> Maybe Value
parseValue symbol
    | isDigit symbol = getDigitValue . digitToInt $ symbol
    | otherwise = getBroadwayValue symbol
    where
        -- | If digit satisfies the required conditions -
        -- calculate the index in the 'Value' enum, wrap it with 'Just' and return.
        -- otherwise - 'Nothing' returns.
        getDigitValue :: Int -> Maybe Value
        getDigitValue digit
            | digit > 1 && digit < 10 = Just (toEnum $ digit - 2 :: Value)
            | otherwise = Nothing

        -- | Just a shortcut for all 'Value's as a list
        allValues :: [Value]
        allValues = [minBound .. maxBound] :: [Value]

        -- | Broadway 'Value' symbols are "AKQJT".
        -- This method take last five 'Value's' first letter to form this list.
        broadwaySymbols :: [Char]
        broadwaySymbols = [head . show $ s | s <- take 5 $ reverse allValues]

        -- | If given char is present in [AKQJT] list - calc its index in 'Value's enum;
        -- Then wrap it with 'Just' and return.
        -- If char is invalid - return 'Nothing'.
        getBroadwayValue :: Char -> Maybe Value
        getBroadwayValue v = case v `elemIndex` broadwaySymbols of
            Nothing -> Nothing
            Just index -> Just (toEnum $ length allValues - index - 1 :: Value)

-- | This type represents card suit.
data Suit = Hearts
    | Diamonds
    | Clubs
    | Spades deriving (Eq, Show, Enum, Bounded)

-- | This type represents a card - combination of suit and value.
data Card = Card {
    value :: Value,
    suit :: Suit
}

{- | This method gets a char which represents card suit
and returns a 'Suit' wrapped with 'Maybe'.

Char should be a lowercase symbol from "hdcs" list.
Otherwise - 'Nothing' returns.

__Examples:__

@
parseSuit \'h\' = 'Just' 'Hearts'
parseSuit \'c\' = 'Just' 'Clubs'
parseSuit \'x\' = 'Nothing'
@
-}
parseSuit :: Char -> Maybe Suit
parseSuit symbol = case symbol `elemIndex` suitSymbols of
    Nothing -> Nothing
    Just index -> Just (toEnum index :: Suit)
    where
        -- | Just a shortcut for all 'Suit's as a list
        allSuits :: [Suit]
        allSuits = [minBound .. maxBound] :: [Suit]

        -- | 'Suit' symbols are [hdcs].
        -- This method take each 'Suit's' first letter lowercase to form this list.
        suitSymbols :: [Char]
        suitSymbols = [toLower . head . show $ s | s <- allSuits]

