{-# OPTIONS -Wall #-}

module Problem0003 (showAnswer) where

import qualified Data.List.Ordered (minus)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 3"
    --print $ scanl (+) 0 [1..5]
    --print $ take 10 primes
    --print $ take 10 primes'
    --putStrLn "Primes Test 01"
    --print $ take 10000 primes
    --putStrLn "Primes Test 02"
    --print $ take 10000 primes'
    print $ solve


primes = map head $ scanl (\\) [2..] [[p,p+p..] | p <- primes]
            where (\\) = Data.List.Ordered.minus

primes' = [ x | x <- maybePrimes, isReallyPrime x maybePrimes == True]
            where 
                maybePrimes :: [Int]
                maybePrimes = [2,3,5] ++ [ 6*a + b | a <- [1..], b <- [1, 5]]

                isReallyPrime :: Int -> [Int] -> Bool
                isReallyPrime n candidates = all (\x -> n `mod` x /= 0) $ takeWhile ((>=) (truncate $ sqrt $ fromIntegral n)) candidates

solve :: Int
solve = largestPrimeFactor 600851475143

largestPrimeFactor :: Int -> Int
largestPrimeFactor n = maximum $ primeFactors n

primeFactors :: Int -> [Int]
primeFactors n = [ x | x <- takeWhile ((>) limit) primes', n `mod` x == 0]
                where
                    limit = truncate $ sqrt $ fromIntegral n

