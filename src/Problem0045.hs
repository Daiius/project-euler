module Problem0045 (
    showAnswer
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 45"
    print $ isPentagonal 5
    print $ isHexagonal  6
    print $ take 2 $ filter (\x -> (isHexagonal x) && (isPentagonal x)) triangleNumbers
    print $ take 3 $ filter (\x -> (isHexagonal x) && (isPentagonal x)) triangleNumbers

triangleNumbers = [n * (n + 1) `div` 2 | n <- [1..]]

isPentagonal n = (sq^2 == 1 + 24 * n) && ((sq+1) `mod` 6 == 0)
    where
        sq = truncate $ sqrt $ fromIntegral (1 + 24 * n)

isHexagonal n = (sq^2 == 1 + 8*n) && ((sq+1) `mod` 4 == 0)
    where
        sq = truncate $ sqrt $ fromIntegral (1 + 8*n)
