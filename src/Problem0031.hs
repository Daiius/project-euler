module Problem0031 (
    showAnswer
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 31"
    print $ possibleNCoins
    print $ coinCombinations 200
    --let pp = possiblePatterns
    --print $ pp
    --print $ length pp

coins = [200, 100, 50, 20, 10, 5, 2, 1]
ncoins = length coins

possibleNCoins = map (200 `div`) coins

coinSum :: [Int] -> Int
coinSum pattern = sum $ map (\(a, b) -> a * b) $ zip coins pattern



possiblePatterns = [ [a,b,c,d,e,f,g,h]
                        | a <- [0..possibleNCoins !! 0]
                        , b <- [0..possibleNCoins !! 1]
                        , c <- [0..possibleNCoins !! 2]
                        , d <- [0..possibleNCoins !! 3]
                        , e <- [0..possibleNCoins !! 4]
                        , f <- [0..possibleNCoins !! 5]
                        , g <- [0..possibleNCoins !! 6]
                        , h <- [0..possibleNCoins !! 7]
                        , coinSum [a,b,c,d,e,f,g,h] == 200
                   ]

-- 
-- coinCombinations :: Int -> Int
coinCombinations n = coinCombinations' n 0

coinCombinations' :: Int -> Int -> Int
coinCombinations' 0 _ = 1
coinCombinations' resting 7 = 1
coinCombinations' resting depth
    = sum [coinCombinations' (nextResting n) (depth + 1) | n <- [0..(possibleNCoins !! depth)], nextResting n >= 0]
        where
            nextResting :: Int -> Int
            nextResting n = resting - (coins !! depth) * n

