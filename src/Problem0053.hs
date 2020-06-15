module Problem0053 (
    answer,
    test
    ) where

import Debug.Trace (trace)

answer :: IO ()
answer = do
    putStrLn "Problem 53"
    print $ length $ filter (1000000 <) $ map (\(n, r) -> nCr' n r) [(n, r) | n <- [1..100], r <- [1..n]]


test :: IO ()
test = do
    putStrLn "Problem 53 Test"
    print $ nCr 3 1
    print $ nCr 3 2
    print $ nCr 4 2
    print $ nCr 23 10
    print $ nCr' 100 50
    let list = filter (1000000 <) $ map (\(n, r) -> trace ("(" ++ show n ++ "," ++ show r ++ ")\o033[1A") nCr' n r) [(n, r) | n <- [1..100], r <- [1..n]]
    putStrLn $ "\n" ++ show (length $ list)
    --print $ filter (1000000 <) $ [nCr n r | n <- [1..100], r <- [1..n]]

nCr' n r = (factorial n) `div` (factorial r * factorial (n-r))
    where
        factorial 1 = 1
        factorial 0 = 1
        factorial n = n * factorial (n-1)

nCr n 1 = n
nCr n r 
    | n == r    = 1
    | otherwise = nCr (n-1) r + nCr (n-1) (r-1)

