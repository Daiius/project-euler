module Problem0038 (
    showAnswer
    ) where

import Data.List (nub, find, notElem)

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 38"
    print $ isConcatinatedProduct 9
    print $ isConcatinatedProduct 192
    print $ isConcatinatedProduct 8
    print $ makeConcatinatedProduct 9327
    let targets = filter isConcatinatedProduct [2..limit]
    print $ targets
    print $ maximum targets

limit = 9999

isConcatinatedProduct :: Int -> Bool
isConcatinatedProduct n = ((length $ makeConcatinatedProduct n) == 9)
                       && ((length $ nub $ makeConcatinatedProduct n) == 9)
                       && ('0' `notElem` (makeConcatinatedProduct n))
    
makeConcatinatedProduct n = head $ dropWhile (\x -> length x < 9) $ scanl1 (++) [show $ n * m | m <- [1..]]
--concatinatedProduct = makeConcatinatedProduct n
