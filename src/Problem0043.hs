module Problem0043 (
    showAnswer
    ) where

import Data.List (permutations)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 43"
    print $ take 10 pandigitalNumbers
    print $ isTarget 1406357289
    print $ targets
    print $ sum targets

targets = filter isTarget pandigitalNumbers

isTarget :: Int -> Bool
isTarget n = (partNumber n 1 `mod`  2 == 0)
          && (partNumber n 2 `mod`  3 == 0)
          && (partNumber n 3 `mod`  5 == 0)
          && (partNumber n 4 `mod`  7 == 0)
          && (partNumber n 5 `mod` 11 == 0)
          && (partNumber n 6 `mod` 13 == 0)
          && (partNumber n 7 `mod` 17 == 0)

partNumber :: Int -> Int -> Int
partNumber n i = read $ take 3 $ drop i $ show n

pandigitalNumbers :: [Int]
pandigitalNumbers = map read $ filter (\x -> x !! 0 /= '0') $ permutations "0123456789"
