{-# LANGUAGE DeriveGeneric #-}
-- | Poker combination and its processing methods
module Game.Combination(
    Combination(..),
    getOccurrences,
    parseSequence,
    parsePartHand,
    parseCombination,
    CombinationName(..)) where

import CardParts.Cards (Card (..))
import CardParts.Values (Value(..))
import CardParts.Suits (Suit(..))
import Data.List ( (\\), sort, sortBy )
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

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
    | RoyalFlush deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON CombinationName
instance ToJSON CombinationName

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
    } deriving (Show, Generic)

instance FromJSON Combination
instance ToJSON Combination

{- | Description of 'Eq' class for 'Combination'.

In case if simple `RankCombination` - equality by name and rank;
In case of complex `PartCombination` - by name, first part and second part;

__Examples:__

@
('RankCombination' 'Pair' $ 'Card' 'Three' 'Spades') == ('RankCombination' 'Pair' $ 'Card' 'Three' 'Heart') = 'True'
('RankCombination' 'Set' $ 'Card' 'Three' 'Spades') == ('RankCombination' 'Pair' $ 'Card' 'Three' 'Heart') = 'False'
('RankCombination' 'Set' $ 'Card' 'Three' 'Spades') == ('RankCombination' 'Straight' $ 'Card' 'Three' 'Heart') = 'False'
@
-}
instance Eq Combination where
    c1@RankCombination {} == c2@RankCombination {} =
        name c1 == name c2 && rank c1 == rank c2
    c1@PartCombination {} == c2@PartCombination {} =
        name c1 == name c2 && part1 c1 == part1 c2 && part2 c1 == part2 c2
    c1 == c2 = name c1 == name c2

{- | Description of 'Ord' class for 'Combination'.

In case if simple `RankCombination` - compare by name and rank;
In case of complex `PartCombination` - by name, first part and second part;

__Examples:__

@
('RankCombination' 'Pair' $ 'Card' 'Three' 'Spades') `compare` ('RankCombination' 'Pair' $ 'Card' 'Three' 'Heart') = 'EQ'
('RankCombination' 'Set' $ 'Card' 'Three' 'Spades') `compare` ('RankCombination' 'Pair' $ 'Card' 'Three' 'Heart') = 'GT'
('RankCombination' 'Set' $ 'Card' 'Three' 'Spades') `compare` ('RankCombination' 'Straight' $ 'Card' 'Three' 'Heart') = 'LT'
@
-}
instance Ord Combination where
    c1@RankCombination{}  `compare` c2@RankCombination{}
        | name c1 == name c2 = rank c1 `compare` rank c2
        | otherwise = name c1 `compare` name c2
    c1@PartCombination{}  `compare` c2@PartCombination{}
        | name c1 /= name c2 = name c1 `compare` name c2
        | part1 c1 == part1 c2 = part2 c1 `compare` part2 c2
        | otherwise = part1 c1 `compare` part1 c2
    c1 `compare` c2 = name c1 `compare` name c2


parseCombination :: [Card] -> Either String Combination
parseCombination cards
    | length cards /= 3 && length cards /= 5 = Left $ "Invalid line length: " ++ show(length cards)
    | null pairs = parseSequence cards
    | otherwise = parsePartHand pairs
        where
            pairs = [ occ | occ@(card, count) <- getOccurrences cards, count > 1 ]


-- | Type shorthand for occurrences counter wrapper
type OccurrencesCounter = [(Card, Int)]

{- | This function gets a list of cards and returns a list of tuples.
First element of which contains certain card, and second element contains number of occurences.

Function takes the head of the list and search for occurrences of its card in the tail.
Then, recursively calls itself for tail with that card removed from it.

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

{- | This function parses occurences data and returns a combination, wrapped with 'Right',
or string err msg wrapped with 'Left'.
The input is list of tuples - [('Card', 'Int')], where first element is a card and seconds is number of its occurrences.
__Examples:__

@
parsePartHand [
     'Card' {value = 'Ace', suit = 'Spades'},
    'Card' {value = 'Jack', suit = 'Clubs'},
    'Card' {value = 'Ten', suit = 'Clubs'},
    'Card' {value = 'Queen', suit = 'Hearts},
    'Card' {value = 'King', suit = 'Hearts},
] = 'Right' 'RankCombination' 'Straight' 'Card' {value = 'Ace', suit = 'Spades'}
@
-}
parsePartHand :: OccurrencesCounter -> Either String Combination
parsePartHand [] = Left "Can't process empty pairs array"
parsePartHand pairs@((yCard, yCount):ys) = case length pairs of
    1 -> singlePairHand
    2 -> multiplePairsHand
    n -> Left $ "Invalid number of pairs: " ++ show n
    where
        -- | In case if only one pair found (pair, set or four of a kind)
        singlePairHand :: Either String Combination
        singlePairHand = case yCount of
            4 -> Right $ RankCombination FourOfAKind yCard
            3 -> Right $ RankCombination Set yCard
            2 -> Right $ RankCombination Pair yCard
            _ -> Left $ "Single pair hand: found invalid number of pairs: " ++ show yCount

        -- | In case if multiple pairs found (two pairs or full house)
        multiplePairsHand :: Either String Combination
        multiplePairsHand = case multiplePairsSum of
            4 -> Right $ PartCombination TwoPairs(maximum $ map fst pairs) (minimum $ map fst pairs)
            5 -> Right $ PartCombination FullHouse (fst . head $ sortedPairsByCount) (fst . last $ sortedPairsByCount)
            _ -> Left $ "Invalid multiple pairs sum: " ++ show multiplePairsSum
            where
                -- | Sum counters to find out if it fits the valid combinations
                multiplePairsSum :: Int
                multiplePairsSum = sum $ map snd pairs

                -- | Sort pairs to be able to get higher lower for combinations, where it matters
                sortedPairsByCount :: OccurrencesCounter
                sortedPairsByCount = sortBy (\(_, a) (_, b) -> b `compare` a) pairs

{- | This function parses sequences in cards line returns a combination, wrapped with 'Right',
or string err msg wrapped with 'Left'.
The input is list of cards - ['Card'].
__Examples:__

@
parseSequence [
    ('Card' {value = 'Ace', suit = 'Spades'}, 2),
    ('Card' {value = 'King', suit = 'Clubs'}, 2)
] = 'Right' 'PartCombination' 'TwoPairs' ('Card' {value = 'Ace', suit = 'Spades') ('Card' {value = 'King', suit = 'Clubs'})
@
-}
parseSequence :: [Card] -> Either String Combination
parseSequence [] = Left "Can't process empty list"
parseSequence cards@(x:xs) = case (isFlush, isSequence, isWheel) of
    ( False, True, False ) -> Right $ RankCombination Straight maxCard
    ( False, False, True ) -> Right $ RankCombination Straight maxCard
    ( True, False, False ) -> Right $ RankCombination Flush maxCard
    ( True, True, False ) -> case value maxCard of
        Ace -> Right $ RankCombination RoyalFlush maxCard
        _ -> Right $ RankCombination StraightFlush maxCard
    ( True, False, True ) -> Right $ RankCombination StraightFlush maxCard
    ( False, False, False ) -> Right $ RankCombination Kicker maxCard
    _ -> Left "Can't parse the combination"
    where
        -- | Simply sort cards
        sortedCards :: [Card]
        sortedCards = sort cards

        -- | Check if the line is a wheel (special variant of straight - A2345)
        isWheel :: Bool
        isWheel = map value sortedCards == [Two, Three, Four, Five, Ace]

        -- Check if the line is a flush (all suits are the same)
        isFlush :: Bool
        isFlush = length cards == 5
            && all ((== suit x) . suit) xs

        -- | Helper function for sequence checking
        getValueNum :: Int -> Int
        getValueNum index = fromEnum . value $ sortedCards !! index

        -- | Check whether sorted elements form a sequence
        isSequence :: Bool
        isSequence = length cards == 5
            && all (== 1) [ getValueNum index - getValueNum (index - 1) | index <- [1 .. length sortedCards - 1] ]

        -- | Get max card for the cards line
        maxCard :: Card
        maxCard
            | isWheel = last $ init sortedCards
            | otherwise = maximum cards



