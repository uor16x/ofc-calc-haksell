-- | Poker combination and its processing methods
module Game.Combination where
import CardParts.Cards (Card)

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
    }

instance Eq Combination where
    c1@RankCombination {} == c2@RankCombination {} =
        name c1 == name c2 && rank c1 == rank c2
    c1@PartCombination {} == c2@PartCombination {} =
        name c1 == name c2 && part1 c1 == part1 c2 && part2 c1 == part2 c2
    c1 == c2 = name c1 == name c2

instance Ord Combination where
    c1@RankCombination{}  `compare` c2@RankCombination{}
        | name c1 == name c2 = rank c1 `compare` rank c2
        | otherwise = name c1 `compare` name c2
    c1@PartCombination{}  `compare` c2@PartCombination{}
        | name c1 /= name c2 = name c1 `compare` name c2
        | part1 c1 == part1 c2 = part2 c1 `compare` part2 c2
        | otherwise = part1 c1 `compare` part1 c2
    c1 `compare` c2 = name c1 `compare` name c2
