module Problem0006 (showAnswer) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 5"
    print $ solve [1..10]
    print $ solve [1..100]

solve :: [Int] -> Int
solve ns = squareOfSum ns - sumOfSquares ns
    where
        squareOfSum :: [Int] -> Int
        squareOfSum ns = total * total
            where
                total = sum ns
        
        sumOfSquares :: [Int] -> Int
        sumOfSquares ns = sum $ map (\x -> x * x) ns
