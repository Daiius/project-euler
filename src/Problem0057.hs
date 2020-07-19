module Problem0057 (
    answer,
    test
    ) where

import Data.Ratio

answer :: IO ()
answer = do
    putStrLn "Problem 57"

test :: IO ()
test = do
    putStrLn "Test Problem 57"
    print $ map fraction [1..5]
    print $ filter hasLongerNumerator $ map fraction [1..20]
    print $ length $ filter hasLongerNumerator $ map fraction [1..1000]

fraction :: Integer -> Ratio Integer
fraction n = 1 + 1 / (fraction' n)
    where
        fraction' :: Integer -> Ratio Integer
        fraction' 1 = 2
        fraction' n = 2 + 1 / (fraction' (n-1))

hasLongerNumerator :: Ratio Integer -> Bool
hasLongerNumerator r = nNumeratorDigits > nDenominatorDigits
    where
        nNumeratorDigits   = length $ show $ numerator r
        nDenominatorDigits = length $ show $ denominator r
