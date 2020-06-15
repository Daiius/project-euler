module Problem0051_0100 (
    answer,
    test,
    solve
    ) where


import qualified Problem0051
import qualified Problem0052
import qualified Problem0053

last2 :: [a] -> [a]
last2 list = drop (length list - 2) list

solve :: IO()
solve = sequence_ $ last2 [
    Problem0051.test,
    Problem0051.answer,
    Problem0052.test,
    Problem0052.answer,
    Problem0053.test,
    Problem0053.answer
    ]

answer :: IO ()
answer = sequence_ [
    Problem0051.answer
    ]

test :: IO ()
test = sequence_ [
    Problem0051.test
    ]

