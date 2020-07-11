module Problem0056 (
    answer,
    test
    ) where

import Data.Char (digitToInt)

answer :: IO ()
answer = do
    putStrLn "Problem 56"
    print $ maximum $ map digitalSum numbers

test :: IO ()
test = do
    putStrLn "Problem 56 Test"
    print $ take 100 numbers
    print $ take 100 $ map digitalSum numbers
    print $ maximum $ map digitalSum numbers

numbers = [ a ^ b | a <- [2..100], b <- [2..100] ]

digitalSum n = sum . (map digitToInt) $ show n
