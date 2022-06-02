{-# LANGUAGE DeriveGeneric #-}
-- | Card value data type and its processing functions
module CardParts.Values(Value(..), parseValue) where

import Data.List (elemIndex)
import Data.Char ( toLower, isDigit, digitToInt )
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

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
    | Ace deriving (Show, Eq, Enum, Ord, Bounded, Generic)

instance FromJSON Value
instance ToJSON Value

-- | Shorthand for 'Value' 'Either' wrapper
type ValueResult = Either String Value

{- | This function gets a char which represents card value
and returns a 'Value' wrapped with 'Maybe'.

Char should be a digit between 2 and 9 or an uppercase symbol from [AKQJT] list.
Otherwise - 'Nothing' returns.

__Examples:__

@
parseValue \'2\' = 'Right' 'Two'
parseValue \'Q\' = 'Right' 'Queen'
parseValue \'z\' = 'Left' "There is no broadway card, which could be represented with \'z\'"
parseValue \'1\' = 'Left' "There is no number card with value 1"
@
-}
parseValue :: Char -> ValueResult
parseValue symbol
    | isDigit symbol = getDigitValue . digitToInt $ symbol
    | otherwise = getBroadwayValue symbol
    where
        -- | If digit satisfies the required conditions -
        -- calculate the index in the 'Value' enum, wrap it with 'Right' and return.
        -- otherwise - 'Left' with err msg returns.
        getDigitValue :: Int -> ValueResult
        getDigitValue digit
            | digit > 1 && digit < 10 = Right (toEnum $ digit - 2 :: Value)
            | otherwise = Left $ "There is no number card with value " ++ show digit

        -- | Just a shortcut for all 'Value's as a list
        allValues :: [Value]
        allValues = [minBound .. maxBound] :: [Value]

        -- | Broadway 'Value' symbols are "AKQJT".
        -- This function take last five 'Value's' first letter to form this list.
        broadwaySymbols :: [Char]
        broadwaySymbols = [head . show $ s | s <- take 5 $ reverse allValues]

        -- | If given char is present in [AKQJT] list - calc its index in 'Value's enum;
        -- Then wrap it with 'Right' and return.
        -- If char is invalid - return 'Left' with err msg.
        getBroadwayValue :: Char -> ValueResult
        getBroadwayValue v = case v `elemIndex` broadwaySymbols of
            Nothing -> Left $ "There is no broadway card, which could be represented with " ++ show v
            Just index -> Right (toEnum $ length allValues - index - 1 :: Value)