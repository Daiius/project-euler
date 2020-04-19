module Problem0020 (
    showAnswer
    ) where

import Data.Function ((&))
import Data.Char (digitToInt)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 20"
    print $ solve (10 :: Integer)
    print $ solve (100 :: Integer)

solve :: Integer -> Int
solve n = factorial n & show & map digitToInt & sum


factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

