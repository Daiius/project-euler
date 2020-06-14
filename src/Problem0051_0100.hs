module Problem0051_0100 (
    answer,
    test,
    solve
    ) where


import qualified Problem0051

solve :: IO()
solve = sequence_ [
    Problem0051.test,
    Problem0051.answer
    ]

answer :: IO ()
answer = sequence_ [
    Problem0051.answer
    ]

test :: IO ()
test = sequence_ [
    Problem0051.test
    ]

