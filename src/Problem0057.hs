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
    print $ map fraction [1..3]

fraction :: Int -> Ratio
fraction n = 1 + 1 % (fraction' n)
    where
        fraction' 1 = 2
        fraction' n = 2 + 1 % (fraction' (n-1))
