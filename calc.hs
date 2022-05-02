import Data.List (elemIndex)
import Data.Char (toLower)
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
    | Ace deriving (Show, Eq, Ord, Bounded)

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