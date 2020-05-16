module Problem0019 (
    showAnswer
    ) where

import Data.Function ((&))

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 19"
    print $ daysInAMonth 1900 Feb
    print $ daysInAMonth 1901 Feb
    print $ daysInAMonth 1904 Feb
    print $ take 5 firstDayOfMonths
    print $ take 4 (
          firstDayOfMonths
        & filter (\(year, _) -> year > 1900)
        & map (\(year, ndays) -> (year, map isSunday ndays))
        )
    print solve

solve :: Int
solve =   firstDayOfMonths 
        & filter (\(year, _) -> year > 1900) 
        & map (\(year, ndays) -> (year, filter isSunday ndays)) 
        & map (\(_, nsundays) -> length nsundays)
        & sum


data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Eq, Enum)

daysInAMonth :: Int -> Month -> Int
daysInAMonth year month
    | month == Feb =
        if year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0) then
            29
        else
            28
    | month == Sep = 30
    | month == Apr = 30
    | month == Jun = 30
    | month == Nov = 30
    | otherwise = 31

daysInAYear :: Int -> Int
daysInAYear year = sum $ map (daysInAMonth year) [Jan .. Dec]

firstDayOfMonths :: [(Int, [Int])]
firstDayOfMonths = [ (year, firstDayOfMonths' year) | year <- [1900..2000] ]
                    where
                        firstDayOfMonths' year = scanl (+) (sum $ map daysInAYear [1900..year-1]) $ map (daysInAMonth year) [Jan .. Nov]
            

isSunday :: Int -> Bool
isSunday n = n `mod` 7 == 6

