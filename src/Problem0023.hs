module Problem0023 (
    showAnswer
    ) where

import Data.List (sort, nub, (\\))

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 23"
    print $ isAbundant 12
    print $ take 10 abundantNumbers
    print $ take 10 allSumsOfANs
    print $ take 100 $ targets
    print $ reverse $ take 100 $ reverse targets
    print $ reverse $ take 10 $ reverse allSumsOfANs
    print solve

-- list all the positive integers which cannot be written as the sum of two abundant numbers
-- up to n
solve :: Int
solve = sum targets

targets = [1..upperLimit] \\ allSumsOfANs

isAbundant :: Int -> Bool
isAbundant n = (sum $ init $ divisors n) > n

divisors :: Int -> [Int]
divisors n = divisors1 ++ reverse divisors2
                where
                    divisors1 = [ x | x <- candidates, n `mod` x == 0]
                    divisors2 = [ n `div` x | x <- divisors1, n `div` x /= x]
                    candidates = [1..(truncate $ sqrt $ fromIntegral n)]

upperLimit = 28123

abundantNumbers = filter isAbundant ns
    where
        ns = [1..upperLimit]

allSumsOfANs :: [Int]
allSumsOfANs = nub $ sort $ filter (upperLimit >=) [ a + b | a <- abundantNumbers, b <- abundantNumbers]
