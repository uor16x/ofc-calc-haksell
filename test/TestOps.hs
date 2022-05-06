module TestOps(generateTests) where

import Test.HUnit
    ( assertEqual,
      Test(TestLabel, TestCase) )

-- | Function for generalized testing purpose
-- | Receives a function of one arg with have to be tested, name and testData
-- | TestData is a triple of description, argument to be tested and expected result
generateTests :: (Eq a, Show a, Show t) => (t -> a) -> [Char] -> [(String, t, a)] -> [Test]
generateTests function functionName testData = [ 
        TestLabel name $ TestCase(assertEqual (getDesc arg) expected (function arg)) | (name, arg, expected) <- testData 
    ]
    where
        getDesc arg = functionName ++ " " ++ show arg