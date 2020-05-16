module Problem0018 (
    showAnswer
    ) where

import Data.Function ((&))

showAnswer :: IO ()
showAnswer = do
    putStrLn "Problem 18"
    print triangle1
    print $ makePaths triangle1
    print $ solve triangle1
    print $ solve triangle2

solve :: [[Int]] -> Int
solve triangle = makePaths triangle & map sum & maximum

data Path = Path {
      depth  :: Int
    , pos    :: Int
    , behind :: [Int]
    }

makePaths :: [[Int]] -> [[Int]]
makePaths triangle = map (\p -> behind p) $ applyNtimes (length triangle - 1) (makePathsHelper triangle) [start]
                    where
                        start = Path 0 0 $ [trigElem triangle 0 0]

-- (Depth, Position)
-- list under construction
makePathsHelper :: [[Int]] -> [Path] -> [Path]
makePathsHelper triangle current = foldl1 (++) $ map (appendPaths triangle) current

appendPaths :: [[Int]] -> Path -> [Path]
appendPaths triangle path = [ Path (depth path + 1) (pos path)     (behind path ++ [trigElem triangle (depth path + 1) (pos path)])
                            , Path (depth path + 1) (pos path + 1) (behind path ++ [trigElem triangle (depth path + 1) (pos path + 1)])]

trigElem :: [[Int]] -> Int -> Int -> Int
trigElem triangle dep ind = triangle !! dep !! ind

applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)


triangle1 = [
      [3]
    , [7, 4]
    , [2, 4, 6]
    , [8, 5, 9, 3]
    ]

triangle2 = [
      [75]
    , [95, 64]
    , [17, 47, 82]
    , [18, 35, 87, 10]
    , [20, 04, 82, 47, 65]
    , [19, 01, 23, 75, 03, 34]
    , [88, 02, 77, 73, 07, 63, 67]
    , [99, 65, 04, 28, 06, 16, 70, 92]
    , [41, 41, 26, 56, 83, 40, 80, 70, 33]
    , [41, 48, 72, 33, 47, 32, 37, 16, 94, 29]
    , [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14]
    , [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57]
    , [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48]
    , [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31]
    , [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
    ]


