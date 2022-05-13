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

data Input = InitialInput { username :: String, strs :: [String] }
    | BoardInput { username :: String, board :: [[Card]] }
    | CombinationInput { username :: String, scoop :: Bool, combinations :: [Combination] } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case length args of
        1 -> case parse (head args) of
                Right parseResult -> print $ encode parseResult
                Left msg -> error msg
        _ -> error "Invalid number of arguments"

parse :: String -> Either String [Input]
parse userInput = do
    board <- mapM parseInputCards $ parseInput userInput
    mapM parseBoard board

parseInput :: String -> [Input]
parseInput args = map (uncurry InitialInput) (read args :: [(String, [String])])

parseInputCards :: Input -> Either String Input
parseInputCards InitialInput { username = username, strs = strs } = case getUserBoard strs of
    Right board -> Right BoardInput {
        username = username,
        board = board
    }
    Left msg -> Left $ "Parsing of input cards failed: " ++ msg
parseInputCards _ = Left "Valid InitialInput should be passed here"

parseBoard :: Input -> Either String Input
parseBoard BoardInput { username = username, board = board } = case parsedCombinations of
    Right result -> Right $ CombinationInput {
        username = username,
        combinations = result,
        scoop = isScoop result
    }
    Left msg -> Left $ "Parsing of combinations failed: " ++ msg
    where
        parsedCombinations = mapM parseCombination board
        isScoop combinations =
            head combinations > head (tail combinations)
            || head (tail combinations) > last combinations
parseBoard _ = Left "Valid BoardInput should be passed here"
