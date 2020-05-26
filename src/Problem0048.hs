module Problem0048 (
    showAnswer
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 0048"
    print $ sum $ selfPowers 10
    print $ lastTenDigits $ sum $ selfPowers 10
    print $ lastTenDigits $ sum $ selfPowers 1000

lastTenDigits x = drop (n - 10) $ show x
    where
        n = length $ show x

--lastTenDigits x = x `mod` 10000000000

selfPowers n = [ x ^ x | x <- [1..n] ]
