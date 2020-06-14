module Problem0051_0100 (
    answer,
    test
    ) where


import qualified Problem0051

answer :: IO ()
answer = sequence_ [
    Problem0051.answer
    ]

test :: IO ()
test = sequence_ [
    Problem0051.test
    ]

