module Problem0047 (
    showAnswer
    ) where

import Data.List (nub)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 47"
    print $ take 10 primes
    print $ map primeFactors [14,15,644,645,646]
    --print $ haveDistinctPrimeFactors [14, 15]
    --print $ haveDistinctPrimeFactors [644,645,646]
    print $ head $ filter (\list -> all (\x -> numberOfPrimeFactors x == 4) list) $ [ [n..n+3] | n <- [1..] ]

--haveDistinctPrimeFactors list = (length listA) == (length listB)
--    where
--        primeFactorsList = map primeFactors list
--        listA = concat primeFactorsList
--        listB = nub listA



--haveNPrimeFactors n npf = numberOfPrimeFactors n == npf

numberOfPrimeFactors n = length $ primeFactors n

primeFactors n = filter (\x -> n `mod` x == 0) $ takeWhile (n >=) primes


primes = filter isPrime' maybePrimes

maybePrimes = [2,3,5] ++ [6*a + b | a <- [1..], b <- [1,5]]

isPrime' n = all (\x -> n `mod` x /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >=) maybePrimes

isPrime n = all (\x -> n `mod` x /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >=) primes
