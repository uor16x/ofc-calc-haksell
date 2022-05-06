-- | Poker combination and its processing methods
module Game.Combination(Combination(..), CombinationName(..)) where

import CardParts.Cards (Card (..))
import CardParts.Values (Value(..))
import CardParts.Suits (Suit(..))
import Data.List ((\\))

-- | Names of combinations enum
data CombinationName = Kicker
    | Pair
    | TwoPairs
    | Set
    | Straight
    | Flush
    | FullHouse
    | FourOfAKind
    | StraightFlush
    | RoyalFlush deriving (Show, Eq, Ord, Bounded, Enum)

-- | This type represents a poker combination.
-- | It is divided by two constructors: for simple combinations (pair, set etc.)
-- | and for combinations with multiple items (two pairs, full house)
data Combination =
    RankCombination {
        name :: CombinationName,
        rank :: Card
    } | PartCombination {
        name :: CombinationName,
        part1 :: Card,
        part2 :: Card
    } deriving (Show)

instance Eq Combination where
    c1@RankCombination {} == c2@RankCombination {} =
        name c1 == name c2 && rank c1 == rank c2
    c1@PartCombination {} == c2@PartCombination {} =
        name c1 == name c2 && part1 c1 == part1 c2 && part2 c1 == part2 c2
    c1 == c2 = name c1 == name c2
-- TODO: add docs
instance Ord Combination where
    c1@RankCombination{}  `compare` c2@RankCombination{}
        | name c1 == name c2 = rank c1 `compare` rank c2
        | otherwise = name c1 `compare` name c2
    c1@PartCombination{}  `compare` c2@PartCombination{}
        | name c1 /= name c2 = name c1 `compare` name c2
        | part1 c1 == part1 c2 = part2 c1 `compare` part2 c2
        | otherwise = part1 c1 `compare` part1 c2
    c1 `compare` c2 = name c1 `compare` name c2

parseCombination :: [Card] -> Combination
parseCombination cards
    | null pairs = RankCombination { name = RoyalFlush, rank = Card Ace Spades }
    | otherwise = RankCombination { name = RoyalFlush, rank = Card Ace Spades }
        where
            pairs = [ occ | occ <- getOccurrences cards, count > 1]

type OccurrencesCounter = [(Card, Int)]

{- | This function gets a list of cards and returns a list of tuples.
First element of which contains certain card, and second element contains number of occurences

__Examples:__

@
getOccurrences [
    'Card' {value = 'Ace', suit = 'Spades'},
    'Card' {value = 'Jack', suit = 'Clubs'},
    'Card' {value = 'Ace', suit = 'Hearts}
] = [(Card {value = Ace, suit = Spades},2),(Card {value = Jack, suit = Clubs},1)]
@
-}
getOccurrences :: [Card] -> OccurrencesCounter
getOccurrences [] = []
getOccurrences (x:xs) = (x, length xSameValue + 1) : getOccurrences(xs \\ xSameValue)
    where
        xSameValue = filter (\y -> value x == value y) xs

parsePairHand :: [Card] -> OccurrencesCounter -> Either String Combination
parsePairHand cards pairs = case length cards of
    1 -> singlePairHand
    2 -> doublePairsHand
    n -> Left $ "Invalid number of pairs: " ++ show n
    where
        singlePairHand :: Either String Combination
        singlePairHand = Left ""

        doublePairsHand :: Either String Combination
        doublePairsHand = Left ""
