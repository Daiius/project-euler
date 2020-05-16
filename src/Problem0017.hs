module Problem0017 (
    showAnswer
    ) where

import Data.Function ((&))

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 17"
    print $ solve [342]
    print $ solve [115]
    print $ numToStr 200
    print $ solve [1..1000]

solve :: [Int] -> Int
solve ns = ns & map numToStr & foldl1 (++) & filter (\c -> c /= ' ' && c /= '-') & length

numToStr :: Int -> String
numToStr n
    | n == 0 = []
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | n == 10 = "ten"
    | n == 11 = "eleven"
    | n == 12 = "twelve"
    | n == 13 = "thirteen"
    | n == 14 = "fourteen"
    | n == 15 = "fifteen"
    | n == 16 = "sixteen"
    | n == 17 = "seventeen"
    | n == 18 = "eighteen"
    | n == 19 = "nineteen"
    | n == 20 = "twenty"
    | 20 < n && n < 30 = combinedNumber n 20
    | n == 30 = "thirty"
    | 30 < n && n < 40 = combinedNumber n 30
    | n == 40 = "forty"
    | 40 < n && n < 50 = combinedNumber n 40
    | n == 50 = "fifty"
    | 50 < n && n < 60 = combinedNumber n 50
    | n == 60 = "sixty"
    | 60 < n && n < 70 = combinedNumber n 60
    | n == 70 = "seventy"
    | 70 < n && n < 80 = combinedNumber n 70
    | n == 80 = "eighty"
    | 80 < n && n < 90 = combinedNumber n 80
    | n == 90 = "ninety"
    | 90 < n && n < 100 = combinedNumber n 90
    | n == 100 = "one hundred"
    | 100 < n && n < 1000 = if n `mod` 100 /= 0 then
                                numToStr (n `div` 100) ++ " hundred and " ++ (numToStr $ n - ((n `div` 100) * 100))
                            else
                                numToStr (n `div` 100) ++ " hundred"
    | n == 1000 = "one thousand"
    | otherwise = "?????"
        where
            combinedNumber n x = numToStr x ++ "-" ++ numToStr (n - x)
