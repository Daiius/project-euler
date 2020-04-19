module Problem0021 (
    showAnswer
    ) where

import Data.Function ((&))

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 21"
    print $ divisors 220
    print $ properDivisors 220
    print $ d 220
    print $ d $ d 220
    print $ (d $ d 220) == 220
    print $ d 6
    print $ d 28
    print $ amicableNumbersUnder 10000
    print $ sum $ amicableNumbersUnder 10000

amicableNumbersUnder :: Int -> [Int]
amicableNumbersUnder n = [2..n] 
                       & filter (\x -> (d $ d x) == x) 
                       & filter (\x -> d x < n) 
                       & filter (\x -> d x /= x)

d :: Int -> Int
d n = properDivisors n & sum

properDivisors :: Int -> [Int]
properDivisors n = init $ divisors n

divisors :: Int -> [Int]
divisors 1 = [1]
divisors 2 = [1,2]
divisors n = divisors1 ++ reverse divisors2
                where
                    divisors1 = [ x | x <- [1..checkLimit1], n `mod` x == 0]
                    checkLimit1 = truncate $ sqrt $ fromIntegral n
                    divisors2 = map (\x -> n `div` x) $ filter (\x -> n `div` x /= x) $ divisors1
