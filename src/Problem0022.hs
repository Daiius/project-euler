module Problem0022 (
    showAnswer
    ) where

import Data.Char (ord)
import Data.List (sort)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 22"
    namesData <- readFile "p022_names.txt"
    putStrLn $ take 10 namesData
    let names = sort $ wordsWhen (==',') $ filter (/='"') namesData
    print $ take 10 $ names
    print $ preScore "COLIN"
    print $ score (938, "COLIN")
    let indexAndNames = zip [1..] names
    print $ take 10 $ indexAndNames
    print $ indexAndNames !! 937
    print $ sum $ map score indexAndNames


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where
                            (w, s'') = break p s'

preScore :: String -> Int
preScore name = sum $ map ((1 - ord 'A' +) . ord) name

score :: (Int, String) -> Int
score (i, name) = i * preScore name

