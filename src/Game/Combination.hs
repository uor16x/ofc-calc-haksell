-- | Poker combination and its processing methods
module Game.Combination where
import CardParts.Values (Value)
import CardParts.Suits (Suit)

data Combination = Kicker
    | Pair { rank :: Value }
    | TwoPairs { p1 :: Value, p2 :: Value }
    | Set { rank :: Value }
    | Straight { high :: Value }
    | Flush { high :: Value, suit :: Suit }
    | FullHouse { triple :: Value, double :: Value }
    | FourOfAKind { rank :: Value }
    | StraightFlush { high :: Value, suit :: Suit }
    | RoyalFlush deriving (Eq, Show, Ord)