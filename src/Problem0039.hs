module Problem0039 (
    showAnswer
    ) where

import Data.List (maximumBy)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 39"
    print $ triangles 120
    print $ numberOfTriangles
    let pmax = maximumBy (\a b -> compare (snd a) (snd b)) numberOfTriangles
    print $ pmax
    print $ triangles $ fst pmax

numberOfTriangles :: [(Int, Int)]
numberOfTriangles = zip [1..] $ map length $ map triangles [1..1000]

triangles :: Int -> [(Int, Int, Int)]
triangles n = [(a, b, n - (a + b)) 
                | a <- [1..n `div` 2]
                , b <- [1..n `div` 2]
                , b < n `div` 2
                , a < b
                , a^2 + b^2 == (n - (a+b))^2
                ]
