module Problem0040 (
    showAnswer
    ) where

import Data.Char (digitToInt)
import Data.List (foldl', product)
import qualified Data.Vector.Unboxed as V


showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 40"
    print $ limit
    print $ take 10 list


    print $ extract
    print $ product $ map (digitToInt . snd) $ extract

extract = take 7 $ filter isTargetIndex $ zip [1..] list

isTargetIndex :: (Int, Char) -> Bool
isTargetIndex (i, c)
    | i == 1 || i == 10 || i == 100 || i == 1000 || i == 10000 || i == 100000 || i == 1000000
        = True
    | otherwise
        = False

extractNth :: Int -> Int
extractNth n = digitToInt (list !! n)


limit = head $ dropWhile (\x -> snd x < 1000000) $ zip [1..] $ scanl1 (+) [ (10^n-1)*n | n <- [1..]]

list = take 1000000 $ foldr (++) [] [ show n | n <- [1..999999] ]
