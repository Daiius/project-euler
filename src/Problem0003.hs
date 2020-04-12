module Problem0003 (showAnswer) where

import qualified Data.List.Ordered (minus)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 3"
    print $ scanl (+) 0 [1..5]
    print $ take 10 primes
    print $ take 10 primes'

primes = map head $ scanl (\\) [2..] [[p,p+p..] | p <- primes]
            where (\\) = Data.List.Ordered.minus

primes' = [2,3,5] ++ [ x | x <- maybePrimes, isReallyPrime x maybePrimes]
            where
                maybePrimes :: [Int]
                maybePrimes = [ 6*a + b | a <- [1..], b <- [1, 5]]

                isReallyPrime :: Int -> [Int] -> Bool
                isReallyPrime n candidates = all (\x -> n `mod` x /= 0) $ takeWhile ((>=) (ceiling $ sqrt $ fromIntegral n)) candidates
