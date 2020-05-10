module Problem0041 (
    showAnswer
    ) where

import Data.List (permutations)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 41"
    print $ isPandigital 2143
    print $ isPandigital 1234
    print $ isPandigital 1123
    print $ take 10 $ primes
    --print $ filter isPandigital $ takeWhile (987654321 > ) primes
    print $ filter isPrime $ map read $ permutations "123456789"
    print $ filter isPrime $ map read $ permutations "12345678"
    print $ filter isPrime $ map read $ permutations "1234567"
    print $ maximum $ filter isPrime $ map read $ permutations "1234567"

isPandigital n = all (\x -> x `elem` (show n)) $ foldr1 (++) [show x | x <- [1..(length $ show n)]]

primes = filter isPrime' maybePrimes

maybePrimes = [2,3,5] ++ [ 6*a+b | a <- [1..], b <- [1,5] ]

isPrime' n = all (\x -> n `mod` x /= 0) $ takeWhile ((>=) (truncate $ sqrt $ fromIntegral n)) maybePrimes

isPrime n = all (\x -> n `mod` x /= 0) $ takeWhile ((>=) (truncate $ sqrt $ fromIntegral n)) primes

