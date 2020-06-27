module Problem0054 (
    answer,
    test
    ) where

--import Data.List.Split
import Data.Char (digitToInt)

answer :: IO ()
answer = do
    putStrLn "Problem 54"

test :: IO ()
test = do
    putStrLn "Problem 54 Test"
    dataStr <- readFile "p054_poker.txt"
    print $ lines dataStr
    print $ readCard "AH"
    print $ parseHands dataStr



data Rank =   HighCard     | OnePair       | TwoPairs 
            | ThreeOfAKind | Strait        | Flush    | FullHouse
            | FourOfAKind  | StraightFlush | RoyalFlush
            deriving (Eq, Ord, Show)

parseHands :: String -> [(Hand, Hand)]
parseHands str = map parseHand $ lines str

parseHand :: String -> (Hand, Hand)
parseHand str = splitAt 5 $ map readCard $ words str

data Card = Card Int Suit
    deriving (Eq, Ord)

showCardNumber :: Int -> String
showCardNumber 14 = "A"
showCardNumber 13 = "K"
showCardNumber 12 = "Q"
showCardNumber 11 = "J"
showCardNumber 10 = "T"
showCardNumber x  = show x

instance Show Card where
    show (Card x s) = showCardNumber x ++ show s

readCard :: String -> Card
readCard s = Card (convert (s !! 0)) (read ([s !! 1]))
    where
        convert 'T' = 10
        convert 'J' = 11
        convert 'Q' = 12
        convert 'K' = 13
        convert 'A' = 14
        convert  x  = digitToInt x

data Suit = H | D | C | S
    deriving (Show, Read, Eq, Ord, Enum)

type Hand = [Card]

