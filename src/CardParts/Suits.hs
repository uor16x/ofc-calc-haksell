-- | Card suit data type and its processing methods
module CardParts.Suits(Suit(..), parseSuit) where

import Data.List (elemIndex)
import Data.Char ( toLower, isDigit, digitToInt )

-- | This type represents card suit.
data Suit = Hearts
    | Diamonds
    | Clubs
    | Spades deriving (Eq, Show, Enum, Bounded, Ord) -- TODO: remove ord after Ord instance implementation for Combinations that requires Suit

{- | This method gets a char which represents card suit
and returns a 'Suit' wrapped with 'Right'.

Char should be a lowercase symbol from "hdcs" list.
Otherwise - 'Left' with err msg returns.

__Examples:__

@
parseSuit \'h\' = 'Right' 'Hearts'
parseSuit \'c\' = 'Right' 'Clubs'
parseSuit \'x\' = 'Left' "There is no card suit marked as 'x'"
@
-}
parseSuit :: Char -> Either String Suit
parseSuit symbol = case symbol `elemIndex` suitSymbols of
    Nothing -> Left $ "There is no card suit marked as " ++ show symbol
    Just index -> Right (toEnum index :: Suit)
    where
        -- | Just a shortcut for all 'Suit's as a list
        allSuits :: [Suit]
        allSuits = [minBound .. maxBound] :: [Suit]

        -- | 'Suit' symbols are [hdcs].
        -- This method take each 'Suit's' first letter lowercase to form this list.
        suitSymbols :: [Char]
        suitSymbols = [toLower . head . show $ s | s <- allSuits]