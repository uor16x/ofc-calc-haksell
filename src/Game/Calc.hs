{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use first" #-}
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

type LineCompareResult = ([String], Int, Int, [(Int, Int)])

data LineResult = LineResult {
    lineType :: LineType,
    combination :: Combination,
    points :: Int,
    totalCombination :: Int,
    totalBonus :: Int
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
    totalDetailed :: (String, [LineCompareResult]),
    total :: Int
} deriving (Generic)

instance FromJSON PlayerCalculations
instance ToJSON PlayerCalculations

type IntPair = (Int, Int)

calcGame :: [PlayerInput] -> [PlayerCalculations]
calcGame playerInputs
  | length playerInputs /= 3 = error "Invalid number of players"
  | length linesResults /= 3 = error "Invalid number of linesResults"
  | otherwise = map mapLinesResults [0..2]
  where
    linesResults :: [(String, [LineCompareResult])]
    linesResults = collectLinesResults [] playerInputs playerInputs

    mapLinesResults :: Int -> PlayerCalculations
    mapLinesResults index = PlayerCalculations {
      player = playerInputs !! index,
      isScoop = scoop $ playerInputs !! index,
      isNextFantasy =
        nextIsFantasy
        (withFantasy $ playerInputs !! index)
        (board $ playerInputs !! index),
      top = LineResult {
        combination = PartCombination FullHouse (Card Two Hearts) (Card Three Hearts), -- top line has index 0,
        lineType = Top,
        points = getPoints Top (PartCombination FullHouse (Card Two Hearts) (Card Three Hearts)),
        totalCombination = fst $ getLinePoints index 0,
        totalBonus = snd $ getLinePoints index 0
      },
      middle = LineResult {
        combination = PartCombination FullHouse (Card Two Hearts) (Card Three Hearts), -- middle line has index 1,
        lineType = Middle,
        points = getPoints Middle (PartCombination FullHouse (Card Two Hearts) (Card Three Hearts)),
        totalCombination = fst $ getLinePoints index 1,
        totalBonus = snd $ getLinePoints index 1
      },
      bottom = LineResult {
        combination = PartCombination FullHouse (Card Two Hearts) (Card Three Hearts), -- bottom line has index 2,
        lineType = Bottom,
        points = getPoints Bottom (PartCombination FullHouse (Card Two Hearts) (Card Three Hearts)),
        totalCombination = fst $ getLinePoints index 2,
        totalBonus = snd $ getLinePoints index 2
      },
      totalDetailed = linesResults !! index,
      total = totalPoints (linesResults !! index)
      }

    combinationByIndex :: PlayerInput -> Int -> Combination
    combinationByIndex playerInput index = board playerInput !! index

    getTotal :: LineCompareResult -> Int
    getTotal (_, combo, bonus, _) = combo + bonus

    totalPoints ::  (String, [LineCompareResult]) -> Int
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
    nextIsFantasy withFantasy _ = False

    getLinePoints :: Int -> Int -> IntPair
    getLinePoints playerIndex lineIndex
      = foldl
        (\(comboAcc, bonusAcc) (_, _, _, debug) -> (comboAcc + fst (debug !! lineIndex), bonusAcc + snd (debug !! lineIndex)))
        (0, 0)
        (snd (linesResults !! playerIndex))

collectLinesResults :: [LineCompareResult] -> [PlayerInput] -> [PlayerInput] -> [(String, [LineCompareResult])]
collectLinesResults acc full (currPlayer:players) =
  (username currPlayer, map (getResult currPlayer) otherInputs) : collectLinesResults (acc ++ map (getResult currPlayer) otherInputs) full players
    where
      otherInputs :: [PlayerInput]
      otherInputs = filter (\player -> username player /= username currPlayer) full

      negateTuple :: (Int, Int) -> (Int, Int)
      negateTuple (a, b) = (negate a, negate b)

      getResult :: PlayerInput -> PlayerInput -> ([String], Int, Int, [(Int, Int)])
      getResult player1 player2 = case find (\(usernames, _, _, _) -> usernames == [username player2, username player1] ) acc of
        Just (usernames, combo, bonus, debug) -> (reverse usernames, negate combo, negate bonus, map negateTuple debug)
        _ -> comparePlayers player1 player2
collectLinesResults acc _ [] = []

comparePlayers :: PlayerInput -> PlayerInput -> LineCompareResult
comparePlayers
  p1@PlayerInput{ username = p1name, board = boardP1, scoop = p1scoop }
  p2@PlayerInput{ username = p2name, board = boardP2, scoop = p2scoop }
    | not (null boardP1) && length boardP1 /= 3 = error "Board has to be of length 0 or 3"
    | not (null boardP2) && length boardP2 /= 3 = error "Board has to be of length 0 or 3"
    | otherwise = (
      [p1name, p2name],
      foldPoints [topPoints, middlePoints, bottomPoints],
      bonusCalculated,
      [
        (fst topPoints, topBonus),
        (fst middlePoints, middleBonus),
        (fst bottomPoints, bottomBonus)
      ]
    ) where
      topPoints :: IntPair
      topPoints = (
        0, 0
        )

      middlePoints :: IntPair
      middlePoints = (
        0, 0
        )

      bottomPoints :: IntPair
      bottomPoints = (
        0, 0
        )

      mirrorPoints :: Int -> (Int, Int)
      mirrorPoints p = (p, negate p)

      getBonusScoop :: Int
      getBonusScoop = case (p1scoop, p2scoop) of
        (True, True) -> 0
        (True, False) -> -1
        (False, True) -> 1
        (False, False) -> error "getBonusDefault should be used this case"

      getBonusDefault :: Combination -> Combination -> Int
      getBonusDefault c1 c2
        | c1 > c2 = 1
        | otherwise = -1

      anyScoop :: Bool
      anyScoop = p1scoop || p2scoop

      topBonus :: Int
      topBonus =0

      middleBonus :: Int
      middleBonus=0

      bottomBonus :: Int
      bottomBonus=0

      summBonus :: Int
      summBonus = topBonus + middleBonus + bottomBonus

      bonusCalculated :: Int
      bonusCalculated = case topBonus + middleBonus + bottomBonus of
        3 -> 6
        (-3) -> -6
        b -> b

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
