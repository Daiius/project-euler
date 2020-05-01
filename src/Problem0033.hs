module Problem0033 (
    showAnswer
    ) where

import Data.Char (digitToInt)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 33"
    print $ take 10 fractions
    print $ take 10 $ filter hasSameDigit fractions
    print $ take 10 $ map removeDuplicateDigits $ filter hasSameDigit fractions
    print $ take 10 $ removeZeros $ map removeDuplicateDigits $ filter hasSameDigit fractions



removeZeros :: [(Int, Int)] -> [(Int, Int)]
removeZeros list = filter (\(a, b) -> a /= 0 && b /= 0) list

removeDuplicateDigits :: (Int, Int) -> (Int, Int)
removeDuplicateDigits (a, b) = toDigits
    where
        a' = show a
        b' = show b
        (ix, iy) = indicesOfRemainingDigit (a, b)
        toDigits = (digitToInt $ a' !! ix, digitToInt $ b' !! iy)
        

indicesOfRemainingDigit :: (Int, Int) -> (Int, Int)
indicesOfRemainingDigit (a, b)
    | isSameDigits 0 0 = (1, 1)
    | isSameDigits 0 1 = (1, 0)
    | isSameDigits 1 0 = (0, 1)
    | isSameDigits 1 1 = (0, 0)
    where
        a' = show a
        b' = show b
        isSameDigits x y = a' !! x == b' !! y

hasSameDigit :: (Int, Int) -> Bool
hasSameDigit (a, b) = a' !! 0 == b' !! 0
                   || a' !! 1 == b' !! 0
                   || a' !! 0 == b' !! 1
                   || a' !! 1 == b' !! 1
    where
        a' = show a
        b' = show b

fractions :: [(Int, Int)]
fractions = [ (a, b) | a <- [10..98], b <- [10..99], a < b ]
