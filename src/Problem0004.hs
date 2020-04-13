{-# OPTIONS -Wall #-}

module Problem0004 (showAnswer) where

-- largest palindromic number made from the product of two 3-digit numbers
-- should be 6 digit number...
-- 6-digit palindromic numbers are rare than all patterns of product of two 3-digit numbers
--

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 4"
    print $ take 10 palinPart
    print $ take 10 palinStrings
    print $ take 10 palindromicNumbers6
    print $ take 10 targetNumbers
    print $ maximum targetNumbers
   

palindromicNumbers6 :: [Int]
palindromicNumbers6 = map read palinStrings
                        
palinStrings :: [String]
palinStrings = map (\s -> s ++ reverse s) palinPart

palinPart :: [String]
palinPart = map show [100..999]


threeDigitNumbers :: [Int]
threeDigitNumbers = [100..999]

isThreeDigitNumbers :: Int -> Bool
isThreeDigitNumbers n = 100 <= n && n <= 999

targetNumbers :: [Int]
targetNumbers = filter isTarget palindromicNumbers6

isTarget :: Int -> Bool
isTarget n = any (\x -> (isModZero x) && (isDivThreeDigitNumber x)) threeDigitNumbers
            where
                isModZero x = mod n x == 0
                isDivThreeDigitNumber x = isThreeDigitNumbers $ (n `div` x)
