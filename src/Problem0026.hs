module Problem0026 (
    showAnswer
    ) where

import Data.Ratio
import Data.Function ((&))
import Data.List (nub, elemIndex, elemIndices, maximumBy, (!!))
import Data.Ord (comparing)
import Debug.Trace

import qualified Data.Vector as V

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 26"
    print $ take 10 fractions
    print $ take 10 $ fractionDigits 7
    print $ subsequences $ take 10 $ fractionDigits 7
    print $ take 10 $ divList 6
    print $ take 10 $ divList 7
    print $ take 10 $ divList 5
    --print $ (\[a,b] -> b - a)
    --      $ elemIndices 2
    --      $ head $ filter (any (1<))
    --      $ map (\x -> map length x) 
    --      $ map (\x -> [elemIndices (x !! j) x | j <- [0..length x - 1]]) 
    --      $ take 10 [take i $ divList 101 | i <- [1..] ]
    -- print $ calcReccuringLength 6
    --print $ calcReccuringLength 5
    --print $ calcReccuringLength 7

    --print $ findFirstDuplicateIndices $ divList 7
    print $ findFirstDuplicateIndices $ divList 7
    print $ findFirstDuplicateIndices $ divList 5
    print $ findFirstDuplicateIndices $ divList 6
   -- print $ map calcReccuringLength' [1..10]
    let rlist = map calcReccuringLength' [1..1000]
    
    print $ rlist
    print $ maximumBy (comparing fst)  $ zip rlist [1..]

-- ex. searchDuplicateBy (==) [1,2,3,4,1] = (0, 4)
--     searchDuplicateBy (==) [3,3,3,3,3] = (0, 1)
--     searchDuplicateBy (==) [1,3,3,3,3] = (1, 1)
--searchDuplicateBy :: (a -> a -> Bool) -> [a] -> (Int, Int)
--searchDuplicateBy f list = case (list !! i) `elemIndex` (drop i list) of
--                            Just x -> 



findFirstDuplicateIndices :: [(Integer, Integer)] -> (Int, Int)
findFirstDuplicateIndices list = findFirstDuplicateIndices' list (0, 1)
    where
        findFirstDuplicateIndices' :: [(Integer, Integer)] -> (Int, Int) -> (Int, Int)
        findFirstDuplicateIndices' list (i, j)
            | (list !! i) == (list !! j)
                = if list !! i == (0,0) then
                    (0, 0)
                  else
                    (i, j)
            | otherwise
                = if i >= j - 1 then
                    findFirstDuplicateIndices' list (0, j + 1)
                  else
                    findFirstDuplicateIndices' list (i + 1, j)

                            
calcReccuringLength' :: Integer -> Int
calcReccuringLength' n = (\(a, b) -> b - a) $ findFirstDuplicateIndices $ divList n


calcReccuringLength :: Integer -> Int
calcReccuringLength n
    | tmpList /= [] = (\[a,b] -> b - a)
                    $ elemIndices 2
                    $ head tmpList
    | otherwise = 0
            where
                tmpList = filter (any (1<))
                        $ map (\x -> map length x)
                        $ map (\x -> [elemIndices (x !! j) x | j <- [0..length x - 1]])
                        $ take 10000 [take i $ divList n | i <- [1..]]


divList :: Integer -> [(Integer, Integer)]
divList n = 
            --takeWhile (/= (0, 0)) 
            {-$-} iterate (nextDivElem n) (10, n)

appendDivList :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
appendDivList n dl
    | snd nxd == 0 = []
    | nxd `elem` dl = []
    | otherwise     = dl ++ [nxd]
        where
            nxd = nextDivElem n $ last dl

nextDivElem ::  Integer -> (Integer, Integer) -> (Integer, Integer)
nextDivElem n (a, b) = (a', b')
    where
        a' = (a `mod` n) * 10
        b' = (a' `div` n) * n


fractionDigits :: Int -> [Int]
fractionDigits n = numbersUnderPoint $ fractions !! (n-1)

fractions :: [Rational]
fractions = [ 1 % n | n <- [1..] ]

numbersUnderPoint :: Rational -> [Int]
numbersUnderPoint r =
    if targetNum /= 0 then
        targetNum : numbersUnderPoint (r * 10)
    else
        []
    where
        targetNum = (`mod` 10) $ truncate $ (fromRational $ (r*10) :: Double)

-- returns start index and length of reccuring cycle
-- if it is not a reccuring sequence, returns (0, 0) ( or () wil be better??)
searchRecurringPoint :: [Int] -> (Int, Int)
searchRecurringPoint numbers = (startIndex, reccuringLength)
    where
        startIndex = 0
        reccuringLength = 0

subsequences numbers = [ drop i $ take j $ numbers | i <- [0..limit-1], j <- [1..limit], i + j <= limit]
    where
        limit = length numbers



