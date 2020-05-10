module Problem0042 (
    showAnswer
    ) where

import Data.Char (ord)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 42"
    wordsData <- readFile "p042_words.txt"
    let words_ = wordsWhen (== ',') $ filter (/= '"') wordsData
    print $ words_
    print $ score "SKY"
    let maxs = 26 * (maximum $ map length words_)
    let triangleWords = filter (isTriangleWord maxs) words_
    print $ triangleWords
    print $ length triangleWords


isTriangleWord maxs s = (score s) `elem` triangleNumbers maxs

triangleNumbers maxs = takeWhile (maxs >) [ n * (n + 1) `div` 2 | n <- [1..] ]


score s = sum $ map score' s
score' c = ((1 - ord 'A' +) . ord) c

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                        where
                            (w, s'') = break p s'

