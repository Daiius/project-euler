module Problem0010 (
      showAnswer
    , test
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 10"
    print $ solve 2000000

primes :: [Int]
primes = [ x | x <- maybePrimes, isReallyPrime x]
    where
        maybePrimes :: [Int]
        maybePrimes = [2,3,5] ++ [6*a+b | a <- [1..], b <- [1,5]]

        isReallyPrime :: Int -> Bool
        isReallyPrime p = all (\x -> p `mod` x /=0) $ takeWhile ((>=) (truncate $ sqrt $ fromIntegral p)) maybePrimes

solve :: Int -> Int
solve n = sum $ takeWhile ((>) n) primes

test :: Int
test = solve 10

