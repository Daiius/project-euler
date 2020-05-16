module Problem0030 (
    showAnswer
    ) where

import Data.Char (digitToInt)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 30"
    let dnp4 = digitNthPowers 4
    print $ dnp4
    print $ sum dnp4
    let dnp5 = digitNthPowers 5
    print dnp5
    print $ sum dnp5

digitNthPowers m = map fst $ filter (\(a,b) -> a == b && a /= 1) $ zip [1..] (map sum powersOfTestDigits)
    where
        limit = fst $ head $ dropWhile (\x -> fst x < snd x) [(10^n - 1, 9^m * n) | n <- [1..]]
        testDigits = map (map digitToInt) $ map show [1..limit]
        powersOfTestDigits = map (map (\x -> (^) x m)) testDigits

