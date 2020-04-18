module Problem0014 (
    showAnswer
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 14"
    print $ collatz 13
    print $ fst solve

collatz :: Int -> [Int]
collatz n = takeWhile ((<) 0) $ iterate collatz' n
            where
                collatz' x
                    | x == 1 = 0
                    | x `mod` 2 == 0 = x `div` 2
                    | otherwise = x * 3 + 1

solve = foldl1 (takeLongerOne) $ zip ns (map collatz ns)
            where
                score (_, cs) = length $ collatz cs
                ns = [1..1000000]
                takeLongerOne a@(n, cs) b@(n', cs')
                    | length cs > length cs' = a
                    | otherwise = b

