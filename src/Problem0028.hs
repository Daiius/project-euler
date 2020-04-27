module Problem0028 (
    showAnswer
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 28"
    let sd5 = spiralDiagonalNumbers 5
    print $ sd5
    print $ sum sd5
    let sd1001 = spiralDiagonalNumbers 1001
    print $ take 10 sd1001
    print $ sum sd1001

spiralDiagonalNumbers :: Int -> [Int]
spiralDiagonalNumbers n = scanl1 (+) $ foldl (++) [1] [ replicate 4 i | i <- [2,4..n - 1]]
