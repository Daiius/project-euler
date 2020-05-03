module Problem0035 (
    showAnswer 
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 35"
    print $ take 3 $ iterate circulize 197
    print $ circulizedNumbers 2555
    print $ take 10 primes
    print $ takeWhile (< 100) $ filter isCircularPrime $ primes
    print $ targets
    print $ length targets


targets = filter (\x -> '0' `notElem` show x) $ takeWhile (< 1000000) $ filter isCircularPrime $ primes


isCircularPrime :: Int -> Bool
isCircularPrime n = (all isPrime $ tail $ circulizedNumbers n)

primes = filter isPrime maybePrimes

isPrime :: Int -> Bool
isPrime n = all (\x -> n `mod` x /= 0) [ x | x <- takeWhile ((>=) (truncate $ sqrt $ fromIntegral n)) maybePrimes]

maybePrimes = [2,3,5] ++ [6*a + b | a <- [1..], b <- [1,5]]



circulizedNumbers :: Int -> [Int]
circulizedNumbers n = take (length $ show n) $ iterate circulize n

circulize :: Int -> Int
circulize n = read (tail n' ++ [head n'])
    where
        n' = show n

