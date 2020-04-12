module Problem0001 (showAnswer) where

import Data.List (sort)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 1"
    --print $ multiples 10
    --print $ sum $ multiples 10
    --print $ sort $ multiples 100
    print solve 
    putStrLn ""

solve = sum $ multiples 1000

multiples :: Int -> [Int]
multiples n = [ x | x <- [1..(n-1)], x `mod` 3 == 0 || x `mod` 5 == 0]





