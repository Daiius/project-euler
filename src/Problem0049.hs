module Problem0049 (
    showAnswer
    ) where

import Data.List (permutations, sort, nub)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 49"
    print $ take 10 primes
    print $ take 10 targetPrimes
    print $ combinations [1,2,3,4] 3
    print $ length $ combinations [1,2,3,4] 4
    print $ length $ combinations [1..24] 3
    print $ length $ sequences 1487
    --print $ filter (\list -> all (list !! 0 ==) list) $ map (\list -> map (\(a, b) -> b - a) list) $ map (\list -> zip list $ drop 1 list) $ map sort $ sequences 1487
    print $ filter isUnusualII $ sequences 1487
    print $ select [1,2,3,4]
    print $ permutations' [1,2,3,4]
    print $ permutation 3 [1,2,3,4]
    print $ length $ combinations' 3 $ permutations' "1487"
    print $ map sort $ filter isUnusualII $ sequences' 1487
    print $ map sort $ filter isUnusualI $ filter isUnusualII $ sequences' 1487
    print $ map sort $ filter isUnusualI $ filter isUnusualII $ sequences' 1001
    print $ map sort $ filter isUnusualI $ filter isUnusualII $ sequences' 1063
    print $ filter isTarget [1000..9999]
    print $ nub $ map (minimum . permutations . show) $ filter isTarget [1000..9999]
    print $ map sort $ filter isUnusualI $ filter isUnusualII $ sequences' 2699

isTarget n = (map sort $ filter isUnusualI $ filter isUnusualII $ sequences' n) /= []

isUnusualII list = (diffList !! 0 /= 0) && (all ((diffList !! 0) ==) diffList)
    where
        sdList = sort list
        diffList = map (\(a, b) ->  b - a) $ zip sdList $ drop 1 sdList

isUnusualI list = all isPrime list

sequences :: Int -> [[Int]]
sequences n = combinations (map read $ permutations $ show n) 3

sequences' :: Int -> [[Int]]
sequences' n = combinations' 3 (map read $ filter (\x -> x !! 0 /= '0') $ permutations' $ show n)

combinations list r = foldr1 (++) [permutations $ take r $ drop i $ list | i <- [0..(length list - r )]]

--combinations' list r =

select :: [a] -> [(a, [a])]
select [x] = [(x, [])]
select (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (select xs)

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs =
    concatMap (\(y, ys) -> map (y:) (permutations' ys)) $ select xs

permutation :: Int -> [a] -> [[a]]
permutation 0 _ = [[]]
permutation n xs =
    concatMap (\(y, ys) -> map (y:) (permutation (n - 1) ys)) $ select xs

combinations' :: Int -> [a] -> [[a]]
combinations' n xs = comb n (length xs) xs
    where
        comb 0 _ _ = [[]]
        comb r n a@(x:xs)
            | n == r    = [a]
            | otherwise = map (x:) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs

targetPrimes = takeWhile (9999 >=) $ filter (1000 <) primes

isPrime n = (n > 1) && (all (\x -> n `mod` x /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >=) primes)

primes = filter isPrime' maybePrimes
    where
        maybePrimes = [2,3,5] ++ [ 6*a + b | a <- [1..], b <- [1,5] ]
        isPrime' n  = all (\x -> n `mod` x /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >=) maybePrimes
