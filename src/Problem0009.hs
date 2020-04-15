module Problem0009 (
    showAnswer
    , test
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 9"
    print $ solve 1000

solve :: Int -> Int
solve n = (\(a, b, c) -> a * b * c) $ findSpecialPythagoreanPairs n

findSpecialPythagoreanPairs :: Int -> (Int, Int, Int)
findSpecialPythagoreanPairs n = head [(a, b, c) | a <- ns, b <- ns, c <- ns, a < b, b < c, a^2+b^2==c^2, a + b + c == n]
    where
        ns = [1..1000]

test :: Int
test = solve 12

