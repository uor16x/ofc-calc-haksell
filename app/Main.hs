{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Main where

import OfcCalc ()
import System.Environment ( getArgs, getProgName )
import Game.Combination ( Combination, parseCombination )
import Game.Board (getUserBoard, Board)
import Data.Either (isLeft)
import CardParts.Cards (Card(..))

data Input = InitialInput { username :: String, strs :: [String] }
    | BoardInput { username :: String, board :: [[Card]] }
    | CombinationInput { username :: String, combinations :: [Combination] } deriving Show

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case length args of
        2 -> print "Process"
        _ -> error "Invalid number of arguments"
    print $ head args
    print $ args !! 1
    print $ parseInput $ args !! 1

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
        combinations = result
    }
    Left msg -> Left $ "Parsing of combinations failed: " ++ msg
    where
        parsedCombinations = mapM parseCombination board
parseBoard _ = Left "Valid BoardInput should be passed here"
