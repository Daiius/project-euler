module Problem0007 (showAnswer) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 7"
    print $ take 20 primes
    print $ solve 6
    print $ solve 10001

primes = [ x | x <- maybePrimes, isReallyPrime x]
    where
        maybePrimes :: [Int]
        maybePrimes = [2,3,5] ++ [6*a+b | a <- [1..], b <- [1,5]]

        isReallyPrime :: Int -> Bool
        isReallyPrime n = all (\p -> n `mod` p /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >=) maybePrimes

solve :: Int -> Int
solve n = last $ take n primes

