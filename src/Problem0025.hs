module Problem0025 (
    showAnswer
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 25"
    print $ take 12 fib
    print $ take 12 indexAndFib
    print $ take 12 indexAndNDigits
    print $ solve



fib :: (Integral a) => [a]
fib = map fst $ iterate (\(a, b) -> (b, a + b)) (1, 1)

indexAndFib :: (Integral a) => [(Int, a)]
indexAndFib = zip [1..] fib

countDigits :: (Integral a, Show a) => a -> Int
countDigits n = length $ show n

countDigitsByIAF :: (Integral a, Show a) => (Int, a) -> (Int, Int)
countDigitsByIAF (index, num) = (index, countDigits num)

indexAndNDigits = map countDigitsByIAF indexAndFib

solve :: (Int, Int)
solve = head $ filter (\(_, len) -> len >= 1000) indexAndNDigits

