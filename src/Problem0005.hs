{-# OPTIONS -Wall #-}

module Problem0005 (showAnswer) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 5"
    --print $ gcd' 24 15
    --print $ lcm' 24 15
    --print $ lcmMulti [2,3,4]
    print $ lcmMulti [2..20]
    --print $ lcmMulti [2..10]


gcd' :: Int -> Int -> Int
gcd' a b
    | a < b = gcd' b a
    | b == 0 = a
    | otherwise = gcd (a `mod` b) a

lcm' :: Int -> Int -> Int
lcm' a b = (a * b) `div` (gcd a b)

lcmMulti :: [Int] -> Int
lcmMulti ns = foldr1 lcm' ns

