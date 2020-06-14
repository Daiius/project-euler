module Problem0051 (
    answer,
    test
    ) where

import Data.Char (intToDigit)

import Debug.Trace (trace)

answer :: IO ()
answer = do
    putStrLn "Problem 51"

test :: IO ()
test = do
    putStrLn "Test Test 51"
    --print $ select [1,2,3,4]
    print $ combinations 2 [1,2,3,4]
    print $ replaceN 123 0 [1,2]
    print $ map (\repN -> replaceN 123 repN [1,2]) [0..9]
    print $ map (replaceNIndex 123) (combinations 2 [0..(length $ show 123)-1])
    print $ map (replaceNCombi 123) [1..(length $ show 123)-1]
    print $ replaceNAll 13
    print $ map (map ( filter isPrime ) ) $ replaceNAll 13
    print $ replaceNPrimesCount 13
    print $ replaceNPrimesCountMax 13
    print $ replaceNPrimesCountMaximum 13
    print $ replaceNPrimesCountMax 123
    print $ replaceNPrimesCountMaximum 123
    print $ replaceNPrimesCountMaximum 56003
    --print $ map replaceNPrimesCountMaximum [11..100000]
    print $ head . filter (\(len, _) -> (len >= 8)) $ map (\x -> replaceNPrimesCountMaximum $ trace (show x ++ "\r") x)  $ filter (11 <) primes


numbers = [100]

replaceNPrimesCountMaximum n = maximum $ replaceNPrimesCountMax n

replaceNPrimesCountMax n = map maximum $ replaceNPrimesCount n

replaceNPrimesCount n = map (map (\x -> if (not . null) x then
                                            (length x, minimum x)
                                        else
                                            (0, 0)
                                        )) $ replaceNPrimes n

-- [ number of replacements -> [ replace position -> [ 13, 23, 33, 43, ... ] ], [] ]
replaceNPrimes n = map(map ( filter isPrime ) ) $ replaceNAll n

isPrime n = all (\x -> n `mod` x /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >) primes

primes = filter isReallyPrime maybePrimes
    where
        maybePrimes = [2,3,5] ++ [ 6*a+b | a <- [1..], b <- [1,5] ]
        isReallyPrime n = all (\x -> n `mod` x /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >) maybePrimes

-- 123
-- [ number of replacements -> [ replace positions -> [ 123, 223, 323, ... ] , [ 113, 123, 133, ... ]], [] ]
replaceNAll n = map (replaceNCombi n) [1..(length $ show n)-1]

replaceNCombi n nc = map (replaceNIndex n) (combinations nc [0..(length $ show n)-1])

replaceNIndex n indices
    | 0 `elem` indices  = map (\repN -> replaceN n repN indices) [1..9]
    | otherwise         = map (\repN -> replaceN n repN indices) [0..9]


replaceN :: Int -> Int -> [Int] -> Int
replaceN n repN is = read $ map (
                        \i -> if i `notElem` is then
                            nStr !! i
                        else
                            intToDigit repN
                        ) [0..len-1]
    where
        nStr = show n
        len = length nStr

combinations n list = comb n (length list) list
    where
        comb 0 _ _ = [[]]
        comb r n a@(x:xs)
            | n == r = [a]
            | otherwise = map (x:) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs

--select :: [a] -> [(a, [a])]
--select [] = []
--select [x] = [(x, [])]
--select (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (select xs)

