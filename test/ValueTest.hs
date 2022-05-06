module ValueTest(result) where

import TestOps(generateTests)
import Test.HUnit (Test)
import CardParts.Values ( parseValue, Value(..) ) 

testData :: [([Char], Char, Either String Value)]
testData = [ 
    ( "Should return 2", '2', Right Two ),
    ( "Should return 3", '3', Right Three ),
    ( "Should return 4", '4', Right Four ),
    ( "Should return 5", '5', Right Five ),
    ( "Should return 6", '6', Right Six ),
    ( "Should return 7", '7', Right Seven ),
    ( "Should return 8", '8', Right Eight ),
    ( "Should return 9", '9', Right Nine ),
    ( "Should return T", 'T', Right Ten ),
    ( "Should return J", 'J', Right Jack ),
    ( "Should return Q", 'Q', Right Queen ),
    ( "Should return K", 'K', Right King ),
    ( "Should return A", 'A', Right Ace ),
    ( "Should return Nothing #1", '1', Left "There is no number card with value 1"),
    ( "Should return Nothing #2", '-',Left "There is no broadway card, which could be represented with '-'" ),
    ( "Should return Nothing #3", 'z', Left "There is no broadway card, which could be represented with 'z'" ),
    ( "Should return Nothing #4", 'k', Left "There is no broadway card, which could be represented with 'k'" )
    ]

result :: [Test]
result = generateTests parseValue "parseValue" testData
