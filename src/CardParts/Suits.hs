-- | Card suit data type and its processing methods
module CardParts.Suits(Suit(..), parseSuit) where

import Data.List (elemIndex)
import Data.Char ( toLower, isDigit, digitToInt )

-- | This type represents card suit.
data Suit = Hearts
    | Diamonds
    | Clubs
    | Spades deriving (Eq, Show, Enum, Bounded)

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