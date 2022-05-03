module ValueTest(result) where

import CardParts.Values ( parseValue, Value(..) ) 
import Test.HUnit
    ( assertEqual,
      runTestTT,
      Counts,
      Test(TestLabel, TestCase) )

testData :: [([Char], Char, Maybe Value)]
testData = [ 
    ( "Should return 2", '2', Just Two ),
    ( "Should return 3", '3', Just Three ),
    ( "Should return 4", '4', Just Four ),
    ( "Should return 5", '5', Just Five ),
    ( "Should return 6", '6', Just Six ),
    ( "Should return 7", '7', Just Seven ),
    ( "Should return 8", '8', Just Eight ),
    ( "Should return 9", '9', Just Nine ),
    ( "Should return T", 'T', Just Ten ),
    ( "Should return J", 'J', Just Jack ),
    ( "Should return Q", 'Q', Just Queen ),
    ( "Should return K", 'K', Just King ),
    ( "Should return A", 'A', Just Ace ),
    ( "Should return Nothing #1", '1', Nothing ),
    ( "Should return Nothing #2", '-', Nothing ),
    ( "Should return Nothing #3", 'z', Nothing ),
    ( "Should return Nothing #4", 'k',Nothing )
    ]

-- TODO: generalize this method
generateTests :: String -> [Test]
generateTests method = [ TestLabel name $ TestCase(assertEqual (getDesc arg) expected (parseValue arg)) | (name, arg, expected) <- testData ]
    where
        getDesc arg = method ++ " " ++ show arg

result :: [Test]
result = generateTests "parseValue"
