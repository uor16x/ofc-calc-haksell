module OfcCalc (
    Value(..),
    Suit(..),
    parseValue,
    parseSuit,
    Card
) where

import Data.List (elemIndex)
import Data.Char ( toLower, isDigit )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

parseValue :: Char -> Maybe Value
parseValue symbol
    | isDigit symbol = getDigitValue . digitCharToInt $ symbol
    | otherwise = getBroadwayValue symbol
    where
        digitCharToInt :: Char -> Int
        digitCharToInt c = read [c] :: Int

        getDigitValue :: Int -> Maybe Value
        getDigitValue digit
            | digit > 1 && digit < 10 = Just (toEnum digit :: Value)
            | otherwise = Nothing

        allValues :: [Value]
        allValues = [minBound .. maxBound] :: [Value]

        broadwaySymbols :: [Char]
        broadwaySymbols = [head . show $ s | s <- take 5 $ reverse allValues]

        getBroadwayValue :: Char -> Maybe Value
        getBroadwayValue v = case v `elemIndex` broadwaySymbols of
            Nothing -> Nothing
            Just index -> Just (toEnum $ length allValues - index - 1 :: Value)

data Suit = Hearts
    | Diamonds
    | Clubs
    | Spades deriving (Eq, Show, Enum, Bounded)

data Card = Card {
    value :: Value,
    suit :: Suit
}

parseSuit :: Char -> Maybe Suit
parseSuit symbol = case suitIndex of
    Nothing -> Nothing
    Just index -> Just (toEnum index :: Suit)
    where
        allSuits = [minBound .. maxBound] :: [Suit]
        suitSymbols = [toLower . head . show $ s | s <- allSuits]
        suitIndex = symbol `elemIndex` suitSymbols

