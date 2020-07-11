module Problem0055 (
    answer,
    test
    ) where

import Debug.Trace (trace)

answer :: IO ()
answer = do
    putStrLn "Problem 55"
    print $ length $ filter isLychrel $ [1..10000]

test :: IO ()
test = do
    putStrLn "Test Problem 55"
    print $ isPalindrome 121
    print $ reverseNum 1234
    print $ 47 + reverseNum 47
    print $ map (\n -> (n, isLychrel n)) [47, 196]
    print $ length $ filter isLychrel $ [1..10000]
    print $ filter isLychrel $ [1..10000]

isLychrel :: Integer -> Bool
isLychrel n = isLychrel' n 50

isLychrel' _ 0 = True
isLychrel' n depth = (not (isPalindrome reverseAdd) && isLychrel' reverseAdd (depth - 1))
    where
        reverseAdd = n + reverseNum n

isPalindrome :: Integer -> Bool
isPalindrome n = n == reverseNum n

reverseNum :: Integer -> Integer
reverseNum   n = read $ reverse $ show n
