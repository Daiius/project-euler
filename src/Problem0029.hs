module Problem0029 (
    showAnswer
    ) where

import Data.List (sort, nub)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 29"
    let dp5 = distinctPowers 5 5
    print $ dp5
    print $ length dp5
    let dp100 = distinctPowers 100 100
    print $ dp100
    print $ length dp100


distinctPowers :: Integer -> Integer -> [Integer]
distinctPowers alim blim = sort $ nub [ a ^ b | a <- [2..alim], b <- [2..blim] ]
