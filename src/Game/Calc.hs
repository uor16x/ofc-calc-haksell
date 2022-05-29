{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module Game.Calc(calcPlayer, PlayerInput(..), LineResult, LineType(..), PlayerCalculations(..), comparePairOfPlayers, updateTotals) where
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

data PlayerInput = PlayerInput {
    username :: String,
    board :: [Combination]
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

calcPlayer :: PlayerInput -> PlayerCalculations
calcPlayer input@PlayerInput{ board = (top:middle:bottom:xs) } = PlayerCalculations {
    player = input,
    top = getLineResult Top top,
    middle = getLineResult Middle middle,
    bottom = getLineResult Bottom bottom,
    totalDetailed = (0, 0),
    total = 0
}
calcPlayer _ = error "Invalid usage of calcPlayer"

comparePlayersPoints :: LineResult -> LineResult -> LineCompareResult
comparePlayersPoints line1 line2 = (
  points line1 - points line2,
  if combination line1 == combination line2
    then 0
    else if combination line1 > combination line2
      then 1
      else (-1)
  )

type PairOfPlayers = (PlayerCalculations, PlayerCalculations)
comparePairOfPlayers :: PairOfPlayers -> PairOfPlayers
comparePairOfPlayers (p1, p2) = (
    PlayerCalculations {
      player = player p1,
      top = updateLineResult (top p1) topCompared,
      middle = updateLineResult (middle p1) middleCompared,
      bottom = updateLineResult (bottom p1) bottomCompared,
      totalDetailed = totalDetailedPoints,
      total = uncurry (+) totalDetailedPoints
      },
    PlayerCalculations {
      player = player p2,
      top = updateLineResult (top p2) $ getOppositeResult topCompared,
      middle = updateLineResult (middle p2) $ getOppositeResult middleCompared,
      bottom = updateLineResult (bottom p2) $ getOppositeResult bottomCompared,
      totalDetailed = getOppositeResult totalDetailedPoints,
      total = negate $ uncurry (+) totalDetailedPoints
    }
  ) where
      topCompared = comparePlayersPoints (top p1) (top p2)
      middleCompared = comparePlayersPoints (middle p1) (middle p2)
      bottomCompared = comparePlayersPoints (bottom p1) (bottom p2)

      transformWonAll pointsTuple
        | snd pointsTuple == 3 = (fst pointsTuple, snd pointsTuple + 3)
        | snd pointsTuple == (-3) = (fst pointsTuple, snd pointsTuple - 3)
        | otherwise = pointsTuple

      totalDetailedPoints = transformWonAll $
        topCompared `addTuples` middleCompared `addTuples` bottomCompared

      getOppositeResult (combPoints, bonusPoints) = (negate combPoints, negate bonusPoints)

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

addTuples r1 r2 = (fst r1 + fst r2, snd r1 + snd r2)
