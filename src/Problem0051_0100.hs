module Problem0051_0100 (
    answer,
    test,
    solve
    ) where

import Data.List (transpose)

import qualified Problem0051
import qualified Problem0052
import qualified Problem0053
import qualified Problem0054
import qualified Problem0055
import qualified Problem0056
import qualified Problem0057

last2 :: [a] -> [a]
last2 list = drop (length list - 2) list

solve :: IO()
solve = sequence_ $ last2 $ interleave tests answers

interleave xs ys = concat (transpose [xs, ys])

answer :: IO ()
answer = sequence_ answers

test :: IO ()
test = sequence_ tests

answers = [
    Problem0051.answer,
    Problem0052.answer,
    Problem0053.answer,
    Problem0054.answer,
    Problem0055.answer,
    Problem0056.answer,
    Problem0057.answer
    ]


tests = [
    Problem0051.test,
    Problem0052.test,
    Problem0053.test,
    Problem0054.test,
    Problem0055.test,
    Problem0056.test,
    Problem0057.test
    ]

