module Problem0002 (showAnswer) where

import Data.Bits ((.&.))

showAnswer ::IO()
showAnswer = do
    putStrLn "Problem 2"
    --print $ map fst $ take 10 $ iterate advanceFibonacci (1,2)
    --print $ last $ take 32 $ iterate advanceFibonacci (1,2)
    print solve
    putStrLn ""

solve = sum $ fibonacciesEven


fibonacciesEven :: [Int]
fibonacciesEven = filter (\a -> (a .&. 0x01) == 0) fibonaccies

fibonaccies = map fst $ take 32 $ iterate advanceFibonacci (1,2)

advanceFibonacci :: (Int, Int) -> (Int, Int)
advanceFibonacci (a,b) = (b, a+b)


