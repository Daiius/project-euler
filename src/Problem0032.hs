module Problem0032 (
    showAnswer
    ) where

import Data.List (nub)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 32"
    print $ divisors 10
    print $ divisors1 10
    print $ zip (divisors1 10) $ map (10 `div`) (divisors1 10)
    print $ listMMPPatterns 10

    print $ listMMPPatterns 7254
    print $ filter isPandigital $ listMMPPatterns 7254

    print $ filter (\x -> x /= []) $ map listPandigitalMMPPatterns [1000..99999]

    print $ sumOfProducts listAllPanditigalMMPPatterns

sumOfProducts :: [(Int, Int, Int)] -> Int
sumOfProducts mmpPatterns = sum $ nub $ map (\(a, b, c) -> c) mmpPatterns

listAllPanditigalMMPPatterns :: [(Int, Int, Int)]
listAllPanditigalMMPPatterns = concat $ filter (\x -> x /= []) $ map listPandigitalMMPPatterns [1000..9999]

listPandigitalMMPPatterns n = filter isPandigital $ listMMPPatterns n

-- Number -> (Multiplicand, Multiplier, Product)
listMMPPatterns :: Int -> [(Int, Int, Int)]
listMMPPatterns n = [(a, n `div` a, n) | a <- divisors1 n]

isPandigital :: (Int, Int, Int) -> Bool
isPandigital (a, b, c) = ('0' `notElem` numbers) && (length numbers == 9) && ((length $ nub numbers) == 9)
    where
        numbers = show a ++ show b ++ show c


divisors :: Int -> [Int]
divisors n = divisors1 n ++ (reverse $ divisors2 n)
divisors1 n = filter (\x -> mod n x == 0) [1..(truncate $ sqrt $ fromIntegral n)]
divisors2 n = [ n `div` d | d <- divisors1 n, n `div` d /= d]
