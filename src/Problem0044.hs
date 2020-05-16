module Problem0044 (
    showAnswer
    ) where

import Data.List (minimumBy)

import qualified Data.Vector.Unboxed as V


showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 44"
    print $ take 10 pentagonalNumbers
    print $ take 10 searchIndices
    print $ take 10 $ searchTargets
    print $ head targets
    print $ targets
    print $ findMinD

limit = 5000

findMinD = minimumBy (\(a, b)  (c, d) -> compare (d - c) (b - a)) targets

targets = filter isDPentagonal $ searchTargets

isDPentagonal (a, b) = isPentagonal (b - a) && isPentagonal (b + a)
                  --((b - a) `elem` (pentagonalNumbers))
                  --  && ((b + a) `elem` (pentagonalNumbers))

isPentagonal :: Int -> Bool
isPentagonal n = isSquare (1 + 24*n)
              && ((1 + (truncate $ sqrt $ fromIntegral (1 + 24*n))) `mod` 6 == 0)

isSquare n = if (n `mod` 256) `elem` squareModList then
                m^2 == n
             else
                False
             where
                m = truncate $ sqrt $ fromIntegral n

squareModList = [ 0, 1, 4, 9, 16, 17, 25, 33, 36, 41, 49, 57, 64, 65, 68, 73, 81, 89, 97, 100, 105, 113, 121, 129, 132, 137, 144, 145, 153, 161, 164, 169, 177, 185, 193, 196, 201, 209, 217, 225, 228, 233, 241, 249]

searchTargets = map (\(a, b) -> (pentagonalNumbers !! a, pentagonalNumbers !! b)) searchIndices

searchIndices :: [(Int, Int)]
searchIndices = takeWhile (\(a, b) -> a < limit && b < limit) $ iterate searchIndices' (0, 1)

searchIndices' :: (Int, Int) -> (Int, Int)
searchIndices' (a, b)
    | a == b - 1
        = (    0, b + 1)
    | otherwise
        = (a + 1, b    )

pentagonalNumbers = [n * (3*n - 1) `div` 2 | n <- [1..limit]]
