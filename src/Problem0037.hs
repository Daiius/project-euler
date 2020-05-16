module Problem0037 (
    showAnswer
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 37"
    print $ take 10 primes'
    print $ isTruncatablePrime 13
    print $ truncatedFromLeft 13
    let truncatablePrimes = take 11 $ filter isTruncatablePrime primes'
    print $ truncatablePrimes
    print $ sum truncatablePrimes

isTruncatablePrime :: Int -> Bool
isTruncatablePrime n = isTPfromLeft n && isTPfromRight n

isTPfromLeft :: Int -> Bool
isTPfromLeft n = all isPrime $ truncatedFromLeft n
truncatedFromLeft :: Int -> [Int]
truncatedFromLeft n = [ read $ drop x $ show n | x <- [1..(length $ show n) - 1] ]

isTPfromRight :: Int -> Bool
isTPfromRight n = all isPrime $ truncatedFromRight n
truncatedFromRight :: Int -> [Int]
truncatedFromRight n = [ read $ take x $ show n | x <- [1..(length $ show n) - 1] ]

primes' = filter (> 10) primes

primes = filter isPrime maybePrimes

maybePrimes = [2,3,5] ++ [ 6*a + b | a <- [1..], b <- [1,5] ]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all (\x -> n `mod` x /= 0) $ takeWhile (<= (truncate $ sqrt $ fromIntegral n)) maybePrimes

