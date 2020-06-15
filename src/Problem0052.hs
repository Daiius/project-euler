module Problem0052 (
    answer,
    test
    ) where

import Debug.Trace (trace)

answer :: IO ()
answer = do
    putStrLn "Problem 52"
    print $ head $ filter isTarget [100..]

test :: IO ()
test = do
    putStrLn "Problem 52 test"
    print $ select [1,2,3,4]
    print $ permutations [1,2,3,4]
    print $ permuGroup 1023
    print $ head $ filter (\x -> isTarget (trace (show x ++ "\o033[1A") x) ) [100..]

isTarget n = all (\x -> x * n `elem` g) [2..6]
    where
        g = permuGroup n

permuGroup :: (Show a, Read a) => a -> [a]
permuGroup n = map read $ filter (\xs -> xs !! 0 /= '0') $ permutations (show n)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = concatMap (\(y, ys) -> map (y:) (permutations ys)) $ select xs

select :: [a] -> [(a, [a])]
select [] = []
select [x] = [(x, [])]
select (x:xs) = (x, xs) : (map (\(y, ys) -> (y, [x] ++ ys)) $ select xs)
