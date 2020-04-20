module Problem0026 (
    showAnswer
    ) where

import Data.Ratio

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 26"
    print $ take 10 fractions
    print $ take 10 $ numbersUnderPoint $ fractions !! 6


fractions :: [Rational]
fractions = [ 1 % n | n <- [1..] ]

numbersUnderPoint :: Rational -> [Int]
numbersUnderPoint r =
    if targetNum /= 0 then
        targetNum : numbersUnderPoint (r * 10)
    else
        []
    where
        targetNum = (`mod` 10) $ truncate $ (fromRational $ (r*10) :: Double)

                            
