module Problem0051_0100 (
    answer,
    test,
    solve
    ) where


import qualified Problem0051
import qualified Problem0052
import qualified Problem0053
import qualified Problem0054

last2 :: [a] -> [a]
last2 list = drop (length list - 2) list

solve :: IO()
solve = sequence_ $ last2 [
    Problem0051.test,
    Problem0051.answer,
    Problem0052.test,
    Problem0052.answer,
    Problem0053.test,
    Problem0053.answer,
    Problem0054.test,
    Problem0054.answer
    ]

answer :: IO ()
answer = sequence_ [
    Problem0051.answer
    ]

test :: IO ()
test = sequence_ [
    Problem0051.test
    ]

