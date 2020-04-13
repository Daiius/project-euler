module Problem0003 (showAnswer) where

import qualified Data.List.Ordered (minus)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 3"
    print $ scanl (+) 0 [1..5]
    print $ take 10 primes
    print $ take 10 primes'
    putStrLn "Primes Test 01"
    print $ take 10000 primes
    putStrLn "Primes Test 02"
    print $ take 10000 primes'


primes = map head $ scanl (\\) [2..] [[p,p+p..] | p <- primes]
            where (\\) = Data.List.Ordered.minus

primes' = [ x | x <- maybePrimes, isReallyPrime x maybePrimes == True]

--primes'' = foldl (++) 

maybePrimes :: [Int]
maybePrimes = [2,3,5] ++ [ 6*a + b | a <- [1..], b <- [1, 5]]

isReallyPrime :: Int -> [Int] -> Bool
isReallyPrime n candidates = all (\x -> n `mod` x /= 0) $ takeWhile ((>=) (truncate $ sqrt $ fromIntegral n)) candidates
