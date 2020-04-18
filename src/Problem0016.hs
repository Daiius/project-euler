module Problem0016 (
    showAnswer
    ) where

import Data.Char
import Data.Function ((&))

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 16"
    print $ solve 15
    print $ solve 1000
    print $ solve' 1000

solve :: Int -> Int
solve n = sum $ map digitToInt $ show $ 2^n

solve' :: Int -> Int
solve' n = 2^n & show & map digitToInt & sum

