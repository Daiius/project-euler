module Problem0027 (
    showAnswer
    ) where

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 27"
    print $ take 10 primes
    print $ take 10 $ quadratics 1 41
    let qprimes1 = takeWhile isPositivePrime $ quadratics 1 41
    print $ qprimes1
    print $ length qprimes1
    print $ pickMaximumPCLength

pickMaximumPCLength = foldl1 (
                        \x y -> if snd x > snd y then
                                    x
                                else
                                    y
                      ) pcLengths

pcLengths = [((a, b), primeChainLength a b) | a <- [-999..999], b <- [-1000..1000] ]

primeChainLength a b = length $ takeWhile isPositivePrime $ quadratics a b

primes = filter isReallyPrime maybePrimes
    where
        maybePrimes = [2,3,5] ++ [ 6*a + b | a <- [1..], b <- [1,5] ]
        isReallyPrime n = all (\x -> n `mod` x /= 0)
                        $ takeWhile ((>=) (truncate $ sqrt $ fromIntegral n)) maybePrimes

isPositivePrime n = n > 0 && isPrime n

isPrime n = all (\x -> n `mod` x /= 0)
          $ takeWhile ((>=) (truncate $ sqrt $ fromIntegral n)) primes

quadratics a b = [ n*n + a*n + b | n <- [0..] ]
