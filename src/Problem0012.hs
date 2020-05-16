module Problem0012 (
    showAnswer
    ) where

import Debug.Trace

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 12"
    print $ take 10 triangleNumbers
    print $ take 10 numberOfDivisorsOfTNs
    --print $ take 10 $ filter (\n -> 100 < snd n) numberOfDivisorsOfTNs
    print $ head $ filter (\n -> 500 < snd n) $ numberOfDivisorsOfTNs

solve :: Int
solve = 10



triangleNumbers :: [Int]
triangleNumbers = scanl (+) 1 [2..]

divisors :: Int -> (Int, [Int])
divisors n = (n, divisorsList)
            where
                divisorsList = divisorsList' ++ [ n `div` x | x <- divisorsList', n `div` x /= x]
                divisorsList' = filter (\x -> mod n x == 0) ([1.. (truncate $ sqrt $ fromIntegral n + 1)])

divisorsOfTNs :: [(Int, [Int])]
divisorsOfTNs = map divisors triangleNumbers

numberOfDivisorsOfTNs :: [(Int, Int)]
numberOfDivisorsOfTNs = map (\(n, ds) -> (n, length ds)) divisorsOfTNs



