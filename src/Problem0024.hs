module Problem0024 (
    showAnswer
    ) where

import Data.List (permutations, sortOn)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 24"
    print $ lexPerms "012"
    print $ lexPerms "0123456789" !! (1000000-1)


lexPerms :: String -> [String]
lexPerms s = sortOn (\x -> read x :: Int) $ permutations s
