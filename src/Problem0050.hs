module Problem0050 (
    showAnswer
    ) where

import Data.List (sortBy, tails, findIndices)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 50"
    print $ take 10 primes
    print $ sumOfPrimes 100
    print $ consecutivePrimes 1000
    let tmp1 = map (map (\x -> (isPrime x, x))) $ consecutivePrimes 1000
    print $ tmp1
    print $ map index tmp1
    print $ maximum $ map index (tmp1)
    let tmp2 = map (map (\x -> (isPrime x, x))) $ consecutivePrimes 1000000
    print $ maximum $ map index tmp2
    --print $ filter isPrime $ sumOfPrimes 100
    --print $ filter isPrime $ sumOfPrimes 1000
    --print $ consecutivePrimesAndSum 20
    --print $ head $ filter (\(_, s) -> isPrime s) $ consecutivePrimesAndSum 20
    --print $ primesBySumOfConsecutivePrimes 1000
    --print $ (\(list, s) -> (length list, s)) $ primesBySumOfConsecutivePrimes 1000
    --print $ (\(list, s) -> (length list, s)) $ primesBySumOfConsecutivePrimes 10000
    --print $ (\(list, s) -> (length list, s)) $ primesBySumOfConsecutivePrimes 100000

index x = if null $ ind x then
            (0,0)
          else
            (last $ ind x, snd (x !! (last $ ind x)))
    where
        ind = findIndices(fst)

primesBySumOfConsecutivePrimes n =  head $ filter (\(_, s) -> isPrime s) $ consecutivePrimesAndSum n

sumOfPrimes n = takeWhile (n >) $ scanl1 (+) primes

consecutivePrimesAndSum n = filter (\(_, s) -> s < n) $ map (\list -> (list, sum list)) $ consecutivePrimes n

--consecutivePrimes :: Int -> [[Int]]
--consecutivePrimes n = [take j $ drop i $ targetPrimes | j <- [nPrimes,(nPrimes-1)..1], i <-[0..nPrimes], i + j <= nPrimes]
--    where
--       targetPrimes = takeWhile (n `div` 2 >) primes
--        nPrimes = length targetPrimes

consecutivePrimes :: Int -> [[Int]]
consecutivePrimes n = map (takeWhile (n >) . scanl1 (+)) (tails $ takeWhile (n >) primes)

primes :: [Int]
primes = filter isPrime' maybePrimes
    where
        maybePrimes = [2,3,5] ++ [6*a + b | a <- [1..], b <- [1,5]]
        isPrime' n = all (\x -> n `mod` x /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >=) maybePrimes

isPrime :: Int -> Bool
isPrime n = all (\x -> n `mod` x /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >=) primes
