{-# LANGUAGE DeriveGeneric #-}
module Main where

import OfcCalc ()
import System.Environment ( getArgs, getProgName )
import Game.Combination ( Combination, parseCombination )
import Game.Board (getUserBoard, Board)
import Data.Either (isLeft)
import CardParts.Cards (Card(..))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Game.Calc (PlayerInput (..), PlayerCalculations(..))
import Game.Calc ( PlayerInput(PlayerInput), comparePlayers, calcGame )
import Debug.Trace (traceIO, trace)

data Input = InitialInput { username :: String, withFantasy :: Bool, strs :: [String] }
    | BoardInput { username :: String,  withFantasy :: Bool, board :: [[Card]] }
    | CombinationInput { username :: String, scoop :: Bool, withFantasy :: Bool, combinations :: [Combination] } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case length args of
        1 -> case parse (head args) of
                -- Right parseResult -> trace "zxc" (print $ parseInput parseResult)
                Right parseResult -> print $ encode parseResult
                Left msg -> error msg
        _ -> error "Invalid number of arguments"

parse :: String -> Either String [Game.Calc.PlayerCalculations]
parse userInput = do
    board <- mapM parseInputCards $ parseInput userInput
    parsedBoard <- mapM parseBoard board
    playersCalculated <- mapM calcBoard parsedBoard
    Right $ calcGame playersCalculated
      where
        calcBoard :: Input -> Either String Game.Calc.PlayerInput
        calcBoard inp = Right $
          Game.Calc.PlayerInput 
          (username inp)
          (combinations inp)
          (scoop inp)
          (withFantasy inp)

parseInput :: String -> [Input]
parseInput args =
  map
  (\(name, withFantasy, board) -> InitialInput name withFantasy board)
  (read args :: [(String, Bool, [String])])

parseInputCards :: Input -> Either String Input
parseInputCards InitialInput { username = username,  withFantasy = withFantasy, strs = strs } = case getUserBoard strs of
    Right board -> Right BoardInput {
        username = username,
        withFantasy = withFantasy,
        board = board
    }
    Left msg -> Left $ "Parsing of input cards failed: " ++ msg
parseInputCards _ = Left "Valid InitialInput should be passed here"

parseBoard :: Input -> Either String Input
parseBoard BoardInput { username = username, withFantasy = withFantasy, board = board } = case parsedCombinations of
    Right result -> Right $ CombinationInput {
        username = username,
        combinations = result,
        withFantasy = withFantasy,
        scoop = isScoop result
    }
    Left msg -> Left $ "Parsing of combinations failed: " ++ msg
    where
        parsedCombinations = mapM parseCombination board
        isScoop combinations =
            head combinations > head (tail combinations)
            || head (tail combinations) > last combinations
parseBoard _ = Left "Valid BoardInput should be passed here"
