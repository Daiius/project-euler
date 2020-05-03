module Problem0036 (
    showAnswer
    ) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 36"
    print $ isPalindrome "1001001"

    let target = filter isTarget [1,3..1000000]
    print $ sum target

isTarget :: Int -> Bool
isTarget n = isPalindrome base10 && isPalindrome base2
    where
        base10 = show n
        base2  = showIntAtBase 2 intToDigit n ""

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s
