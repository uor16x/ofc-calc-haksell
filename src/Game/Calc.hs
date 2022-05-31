{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module Game.Calc(PlayerInput(..), LineResult, LineType(..), PlayerCalculations(..), comparePlayers, calcGame) where
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
    scoop :: Bool,
    withFantasy :: Bool
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
    points :: Int
} deriving (Show, Generic)

instance FromJSON LineResult
instance ToJSON LineResult

data PlayerCalculations = PlayerCalculations {
    player :: PlayerInput,
    top :: LineResult,
    middle :: LineResult,
    bottom :: LineResult,
    isScoop :: Bool,
    isNextFantasy :: Bool,
    totalDetailed :: (String, [([String], Int, Int, [(Int, Int)])]),
    total :: Int
} deriving (Generic)

instance FromJSON PlayerCalculations
instance ToJSON PlayerCalculations

calcGame :: [PlayerInput] -> [PlayerCalculations]
calcGame playerInputs
  | length playerInputs /= 3 = error "Invalid number of players"
  | length linesResults /= 3 = error "Invalid number of linesResults"
  | otherwise = map mapLinesResults [0..2]
  where
    linesResults = collectLinesResults [] playerInputs playerInputs
    mapLinesResults index = PlayerCalculations {
      player = playerInputs !! index,
      isScoop = scoop $ playerInputs !! index,
      isNextFantasy =
        nextIsFantasy
        (withFantasy $ playerInputs !! index)
        (board $ playerInputs !! index),
      top = LineResult {
        combination = combinationByIndex (playerInputs !! index) 0, -- top line has index 0,
        lineType = Top,
        points = getPoints Top (combinationByIndex (playerInputs !! index) 0)
      },
      middle = LineResult {
        combination = combinationByIndex (playerInputs !! index) 1, -- middle line has index 1,
        lineType = Middle,
        points = getPoints Middle (combinationByIndex (playerInputs !! index) 1)
      },
      bottom = LineResult {
        combination = combinationByIndex (playerInputs !! index) 2, -- bottom line has index 2,
        lineType = Bottom,
        points = getPoints Bottom (combinationByIndex (playerInputs !! index) 2)
      },
      totalDetailed = linesResults !! index,
      total = totalPoints (linesResults !! index)
      }
    combinationByIndex playerInput index = board playerInput !! index
    getTotal (_, combo, bonus, _) = combo + bonus
    totalPoints (_, compareResults) = foldl (\acc r -> acc + getTotal r) 0 compareResults

    nextIsFantasy :: Bool -> [Combination] -> Bool
    nextIsFantasy withFantasy (top:middle:bottom:_)
      | withFantasy =
        top >= RankCombination Set (Card Two Hearts)
        || middle >= PartCombination FullHouse (Card Two Hearts) (Card Three Hearts)
        || bottom >= RankCombination FourOfAKind (Card Two Hearts)
      | otherwise =
        top >= RankCombination Pair (Card Queen Hearts)
        || middle >= PartCombination FullHouse (Card Two Hearts) (Card Three Hearts)
        || bottom >= RankCombination StraightFlush (Card Five Hearts)
    nextIsFantasy withFantasy _ = error "Invalid number of lines for fantasy calc"

collectLinesResults :: [([String], Int, Int, [(Int, Int)])] -> [PlayerInput] -> [PlayerInput] -> [(String, [([String], Int, Int, [(Int, Int)])])]
collectLinesResults acc full (currPlayer:players) =
  (username currPlayer, map (getResult currPlayer) otherInputs) : collectLinesResults (acc ++ map (getResult currPlayer) otherInputs) full players
    where
      otherInputs = filter (\player -> username player /= username currPlayer) full

      negateTuple :: (Int, Int) -> (Int, Int)
      negateTuple (a, b) = (negate a, negate b)

      getResult :: PlayerInput -> PlayerInput -> ([String], Int, Int, [(Int, Int)])
      getResult player1 player2 = case find (\(usernames, _, _, _) -> usernames == [username player2, username player2] ) acc of
        Just (usernames, combo, bonus, debug) -> (reverse usernames, negate combo, negate bonus, map negateTuple debug)
        _ -> comparePlayers player1 player2
collectLinesResults acc _ [] = []

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
