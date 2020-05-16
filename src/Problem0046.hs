module Problem0046 (
    showAnswer
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 46"
    --print $ isOddComposite 9
    --print $ isPrime 15
    --print $ isPrime 13
    --print $ isPrime 11
    --print $ isPrime 111
    --print $ isPrime 5489
    --print $ take 100 maybePrimes
    --print $ take 100 primes
    --print $ map isGBsSum [9, 15, 21, 25, 27, 33, 5777]
    --print $ isGBsSum' 5777
    --let test = [5777 - 2 * x^2 | x <- [1..(truncate $ sqrt $ fromIntegral (5777 `div` 2))]]
    --print $ test
    --print $ any isPrime test
    --print $ filter isPrime test
    --print $ take 10 $ filter isOddComposite [2..]
    --print $ head $ filter (not . isGBsSum') $ filter isOddComposite $ [2..10000000]
    print $ head $ filter (not . isGBsSum) $ filter isOddComposite $ [2..]

isGBsSum' n = any isPrime [n - 2 * x^2 | x <- [1..(truncate $ sqrt $ fromIntegral (n `div` 2))]]

isGBsSum n = any (\x -> isSquare (x `div` 2)) [ (n - p) | p <- ps ]
    where
        ps = takeWhile (n >) primes
        --isSquare x = (truncate $ sqrt $ fromIntegral x)^2 == x

isSquare n = if (n `mod` 256) `elem` squareModList then
                m^2 == n
             else
                False
             where
                m = truncate $ sqrt $ fromIntegral n

squareModList = [ 0, 1, 4, 9, 16, 17, 25, 33, 36, 41, 49, 57, 64, 65, 68, 73, 81, 89, 97, 100, 105, 113, 121, 129, 132, 137, 144, 145, 153, 161, 164, 169, 177, 185, 193, 196, 201, 209, 217, 225, 228, 233, 241, 249]




isOddComposite n = (odd n) && ((not . isPrime) n)

isPrime n = all (\x -> n `mod` x /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >=) primes

primes = filter isPrime' maybePrimes

isPrime' n = all (\x -> n `mod` x /= 0) $ takeWhile ((truncate $ sqrt $ fromIntegral n) >=) maybePrimes
maybePrimes = [2, 3, 5] ++ [6*a + b | a <- [1..], b <-[1,5]]

