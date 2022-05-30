{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module Game.Calc(PlayerInput(..), LineResult, LineType(..), PlayerCalculations(..), updateTotals, comparePlayers, collectCompares) where
import CardParts.Cards (Card(..))
import Game.Combination
    ( Combination,
      CombinationName(..),
      Combination(RankCombination),
      Combination(..) )
import CardParts.Values (Value(..))
import CardParts.Suits (Suit(..))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (find)

data PlayerInput = PlayerInput {
    username :: String,
    board :: [Combination],
    scoop :: Bool
} deriving (Show, Generic)

instance FromJSON PlayerInput
instance ToJSON PlayerInput

data LineType = Top | Middle | Bottom deriving (Show, Generic)

instance FromJSON LineType
instance ToJSON LineType

type LineCompareResult = (Int, Int)

data LineResult = LineResult {
    lineType :: LineType,
    combination :: Combination,
    points :: Int,
    result :: LineCompareResult
} deriving (Show, Generic)

updateLineResult :: LineResult -> LineCompareResult -> LineResult
updateLineResult line cmpResult = line { result = cmpResult }

instance FromJSON LineResult
instance ToJSON LineResult

data PlayerCalculations = PlayerCalculations {
    player :: PlayerInput,
    top :: LineResult,
    middle :: LineResult,
    bottom :: LineResult,
    isScoop :: Bool,
    isNextFantasy :: Bool,
    totalDetailed :: LineCompareResult,
    total :: Int
} deriving (Generic)

instance FromJSON PlayerCalculations
instance ToJSON PlayerCalculations

updateTotals :: PlayerCalculations -> PlayerCalculations -> PlayerCalculations
updateTotals prevResult nextResult = prevResult {
  totalDetailed = totalDetailedSummary,
  total = uncurry (+) totalDetailedSummary
  } where
    totalDetailedSummary = totalDetailed prevResult `addTuples` totalDetailed nextResult

-- calcPlayer :: PlayerInput -> PlayerCalculations
-- calcPlayer input@PlayerInput{ board = (top:middle:bottom:xs) } = PlayerCalculations {
--     player = input,
--     top = getLineResult scoop Top top,
--     middle = getLineResult scoop Middle middle,
--     bottom = getLineResult scoop Bottom bottom,
--     scoop = scoop,
--     nextFantasy =
--       not scoop &&
--       (
--         top >= RankCombination Pair (Card Queen Hearts) ||
--         middle >= PartCombination FullHouse (Card Two Hearts) (Card Three Hearts) ||
--         bottom >= RankCombination FourOfAKind (Card Two Hearts)
--       ),
--     totalDetailed = (0, 0),
--     total = 0
-- } where
--   scoop = top > middle || middle > bottom
-- calcPlayer _ = error "Invalid usage of calcPlayer"

-- collectCompares :: [([String], Int, Int)] -> [PlayerInput] -> [(String, Int, Int)]
-- collectCompares :: (String, (t String, b, c)) -> [PlayerInput] -> (String, [([String], Int, Int, [(Int, Int)])])
collectCompares :: [([String], Int, Int, [(Int, Int)])] -> [PlayerInput] -> [(String, [([String], Int, Int, [(Int, Int)])])]
collectCompares acc full@(currPlayer:players) =
  (username currPlayer, map (getResult currPlayer) otherInputs) : collectCompares (acc ++ map (getResult currPlayer) otherInputs) players
    where
      otherInputs = filter (\player -> username player /= username currPlayer) full

      negateTuple :: (Int, Int) -> (Int, Int)
      negateTuple (a, b) = (negate a, negate b)

      getResult :: PlayerInput -> PlayerInput -> ([String], Int, Int, [(Int, Int)])
      getResult player1 player2 = case find (\(usernames, _, _, _) -> username player1 `elem` usernames && username player2 `elem` usernames ) acc of
        Just (usernames, combo, bonus, debug) -> (reverse usernames, negate combo, negate bonus, map negateTuple debug)
        Nothing -> comparePlayers player1 player2
collectCompares acc [] = []

comparePlayers :: PlayerInput -> PlayerInput -> ([String], Int, Int, [(Int, Int)])
comparePlayers
  p1@PlayerInput{ username = p1name, board = (p1top:p1middle:p1bottom:_), scoop = p1scoop }
  p2@PlayerInput{ username = p2name, board = (p2top:p2middle:p2bottom:_), scoop = p2scoop } = (
    [p1name, p2name],
    foldPoints [topPoints, middlePoints, bottomPoints],
    bonusCalculated,
    [
      (fst topPoints, topBonus),
      (fst middlePoints, middleBonus),
      (fst bottomPoints, bottomBonus)
    ]
  ) where
    topPoints = (
      if p1scoop then 0 else getPoints Top p1top,
      if p2scoop then 0 else getPoints Top p2top
      )
    middlePoints = (
      if p1scoop then 0 else getPoints Middle p1middle,
      if p2scoop then 0 else getPoints Middle p2middle
      )
    bottomPoints = (
      if p1scoop then 0 else getPoints Bottom p1bottom,
      if p2scoop then 0 else getPoints Bottom p2bottom
      )

    mirrorPoints :: Int -> (Int, Int)
    mirrorPoints p = (p, negate p)

    getBonus :: Combination -> Combination -> Int
    getBonus c1 c2 = case (p1scoop, p2scoop) of
      (True, True) -> 0
      (True, False) -> -1
      (False, True) -> 1
      (False, False) -> if c1 > c2
        then 1
        else (-1)

    topBonus = getBonus p1top p2top
    middleBonus = getBonus p1middle p2middle
    bottomBonus = getBonus p1bottom p2bottom
    summBonus = topBonus + middleBonus + bottomBonus
    bonusCalculated = case topBonus + middleBonus + bottomBonus of
      3 -> 6
      (-3) -> -6
      b -> b

comparePlayers _ _ = error "Failed to compare"

foldPoints :: [(Int, Int)] -> Int
foldPoints [] = 0
foldPoints ((r1, r2):xs) = (r1 - r2) + foldPoints xs

comparePlayersPoints :: (Bool, Bool) -> LineResult -> LineResult -> LineCompareResult
comparePlayersPoints scoops@(firstScoop, secondScoop) line1 line2 = (
  combo,
  bonus
  ) where
    combo :: Int
    combo = case scoops of
      (True, True) -> 0
      (True, False) -> negate $ points line2
      (False, True) -> points line1
      (False, False) -> points line1 - points line2

    bonus :: Int
    bonus = case scoops of
      (True, True) -> 0
      (True, False) -> -1
      (False, True) -> 1
      (False, False) -> if combination line1 > combination line2
        then 1
        else (-1)

-- type PairOfPlayers = (PlayerCalculations, PlayerCalculations)
-- comparePairOfPlayers :: PairOfPlayers -> PairOfPlayers
-- comparePairOfPlayers (p1, p2) = (
--     PlayerCalculations {
--       player = player p1,
--       isScoop = isScoop p1,
--       isNextFantasy = isNextFantasy p1,
--       top = updateLineResult (top p1) topCompared,
--       middle = updateLineResult (middle p1) middleCompared,
--       bottom = updateLineResult (bottom p1) bottomCompared,
--       totalDetailed = totalDetailedPoints,
--       total = uncurry (+) totalDetailedPoints
--       },
--     PlayerCalculations {
--       player = player p2,
--       isScoop = isScoop p2,
--       isNextFantasy = isNextFantasy p2,
--       top = updateLineResult (top p2) $ getOppositeResult topCompared,
--       middle = updateLineResult (middle p2) $ getOppositeResult middleCompared,
--       bottom = updateLineResult (bottom p2) $ getOppositeResult bottomCompared,
--       totalDetailed = getOppositeResult totalDetailedPoints,
--       total = negate $ uncurry (+) totalDetailedPoints
--     }
--   ) where
--       topCompared = comparePlayersPoints (isScoop p1, isScoop p2) (top p1) (top p2)
--       middleCompared = comparePlayersPoints (isScoop p1, isScoop p2) (middle p1) (middle p2)
--       bottomCompared = comparePlayersPoints (isScoop p1, isScoop p2) (bottom p1) (bottom p2)

--       transformWonAll pointsTuple
--         | snd pointsTuple == 3 = (fst pointsTuple, snd pointsTuple + 3)
--         | snd pointsTuple == (-3) = (fst pointsTuple, snd pointsTuple - 3)
--         | otherwise = pointsTuple

--       totalDetailedPoints = transformWonAll $
--         topCompared `addTuples` middleCompared `addTuples` bottomCompared

--       getOppositeResult (combPoints, bonusPoints) = (negate combPoints, negate bonusPoints)

getLineResult :: LineType -> Combination -> LineResult
getLineResult lineType combination = LineResult {
    lineType = lineType,
    combination = combination,
    points = getPoints lineType combination,
    result = (0, 0)
}

middlePoints :: [Int]
middlePoints = [0, 0, 0, 2, 4, 8, 12, 20, 30, 50]

bottomPoints :: [Int]
bottomPoints = map (\p -> if p > 2 then p `div` 2 else 0) middlePoints

getPoints :: LineType -> Combination -> Int
getPoints Top (RankCombination Pair (Card rank _)) =
    if fromEnum rank < 4 then 0 else fromEnum rank - 3
getPoints Top (RankCombination Set (Card rank _)) = fromEnum rank + 10
getPoints Top _ = 0
getPoints Middle c = middlePoints !! fromEnum (name c)
getPoints Bottom c = bottomPoints !! fromEnum (name c)

addTuples :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addTuples r1 r2 = (fst r1 + fst r2, snd r1 + snd r2)
