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
        2 -> case head args of
            "parse" -> case parse (args !! 1) of
                Right parseResult -> print parseResult
                Left msg -> error msg
            "calc" -> print "calc"
            _ -> error "First argument should be parse | calc"
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
        combinations = result
    }
    Left msg -> Left $ "Parsing of combinations failed: " ++ msg
    where
        parsedCombinations = mapM parseCombination board
parseBoard _ = Left "Valid BoardInput should be passed here"
