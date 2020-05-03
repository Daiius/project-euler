module Problem0033 (
    showAnswer
    ) where

import Data.Char (digitToInt)
import Data.Ratio ((%))

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 33"
    print $ take 10 fractions
    print $ take 10 $ filter hasSameDigit fractions
    print $ take 10 $ map removeDuplicateDigits $ filter hasSameDigit fractions
    print $ take 10 $ removeZeros $ map removeDuplicateDigits $ filter hasSameDigit fractions
    print $ take 10 $ filter isTargetFraction' $ removeZeros $ map removeDuplicateDigits $ filter hasSameDigit fractions
    print $ nonTrivialTargets
    print $ product $ map (\((a, b), (_, _)) -> a % b) nonTrivialTargets

type Fraction = (Int, Int)
type Fraction' = ((Int, Int), (Int, Int)) 

nonTrivialTargets = filter isNonTrivial
                  $ filter isTargetFraction' 
                  $ removeZeros 
                  $ map removeDuplicateDigits 
                  $ filter hasSameDigit fractions

isNonTrivial :: Fraction' -> Bool
isNonTrivial ((a, b), (_, _)) = ('0' `notElem` a') && ('0' `notElem` b')
    where
        a' = show a
        b' = show b

isTargetFraction' :: Fraction' -> Bool
isTargetFraction' ((a, b), (a', b')) = a % b == a' % b'

removeZeros :: [Fraction'] -> [Fraction']
removeZeros list = filter (\((_,_), (a, b)) -> a /= 0 && b /= 0) list

removeDuplicateDigits :: Fraction -> Fraction'
removeDuplicateDigits (a, b) = toDigits
    where
        a' = show a
        b' = show b
        (ix, iy) = indicesOfRemainingDigit (a, b)
        toDigits = ((a, b), (digitToInt $ a' !! ix, digitToInt $ b' !! iy))
        

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

hasSameDigit :: Fraction -> Bool
hasSameDigit (a, b) = a' !! 0 == b' !! 0
                   || a' !! 1 == b' !! 0
                   || a' !! 0 == b' !! 1
                   || a' !! 1 == b' !! 1
    where
        a' = show a
        b' = show b

fractions :: [Fraction]
fractions = [ (a, b) | a <- [10..98], b <- [10..99], a < b ]
