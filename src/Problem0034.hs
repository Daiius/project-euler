{-# OPTIONS -Wall #-}

module Problem0034 (
    showAnswer
    ) where

import Data.Function ((&))
import Data.Char (digitToInt)


showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 34"
    print estimateLimit
    print $ isTarget 145
    print $ isTarget 19
    let list = [1..estimateLimit] :: [Int]
    print $ list `seq` take 10 $ reverse list
    
    -- only this part consumes huge amount of memory...
    print $ isTarget 1
    print $ isTarget 2
    print $ isTarget 10
    let targets = filter (\x -> x /= 1 && x /= 2) $ filter isTarget list
    print $ targets
    print $ sum targets


isTarget :: Int -> Bool
isTarget n = (show n & map digitToInt & map factorial & sum) == n

estimateLimit :: Int
estimateLimit = fst $ head $ dropWhile (\(a, b) -> a < b) candidates
    where
        candidates = [(10^n-1, n * factorial 9) | n <- [1..]]


factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial(n - 1)

