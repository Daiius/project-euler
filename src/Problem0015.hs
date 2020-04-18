module Problem0015 (
    showAnswer
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 15"
    print $ solve 2
    print $ solve 20

--solve :: Int -> Int
solve n = combination (n*2) n

combination :: Integer -> Integer -> Integer
combination n r = (foldl1 (*) $ take (fromIntegral r) [n,n-1..1]) `div` (foldl1 (*) [1..r])

