module Problem0054 (
    answer,
    test
    ) where

--import Data.List.Split
import Data.Char (digitToInt)

import Data.List (sort, maximumBy, nub)

import Data.Ord (compare)

answer :: IO ()
answer = do
    putStrLn "Problem 54"
    dataStr <- readFile "p054_poker.txt"
    print $ length $ filter (\(r1, r2) -> match r1 r2 == Win) $ handsToRanks $ parseHands dataStr

test :: IO ()
test = do
    putStrLn "Problem 54 Test"
    dataStr <- readFile "p054_poker.txt"
    print $ lines dataStr
    print $ readCard "AH"
    print $ parseHands dataStr
    let handWithConsequtiveSameSuit = [Card 1 H, Card 2 H, Card 3 H, Card 4 H, Card 5 H]
    print $ handWithConsequtiveSameSuit
    print $ isAllSuitsSame handWithConsequtiveSameSuit
    print $ isConsecutiveNumbers handWithConsequtiveSameSuit
    print $ isRoyalFlush handWithConsequtiveSameSuit
    let royalFlushExample = [Card 10 H, Card 11 H, Card 12 H, Card 13 H, Card 14 H]
    print $ royalFlushExample
    print $ isRoyalFlush royalFlushExample
    print $ isStraightFlush handWithConsequtiveSameSuit
    print $ (HighCard  1 < RoyalFlush)
    print $ (HighCard 1 < HighCard 2)
    print $ (HighCard 10 > HighCard 5)
    let fourOfAKindExample = [Card 4 H, Card 4 S, Card 4 D, Card 4 C, Card 10 H]
    print $ fourOfAKindExample
    print $ isFourOfAKind fourOfAKindExample
    print $ isFourOfAKind royalFlushExample
    let onePairExample = [Card 11 H, Card 11 D, Card 1 H, Card 2 D, Card 3 C]
    print $ onePairExample
    print $ isOnePair onePairExample
    print $ isThreeOfAKind onePairExample
    let threeOfAKindExample = [Card 11 H, Card 11 D, Card 11 C, Card 1 H, Card 2 D]
    print $ threeOfAKindExample
    print $ isThreeOfAKind threeOfAKindExample
    print $ isOnePair threeOfAKindExample
    let twoPairsExample = [Card 11 H, Card 11 D, Card 10 C, Card 10 S, Card 2 D]
    print $ twoPairsExample
    print $ isTwoPairs twoPairsExample
    print $ isTwoPairs onePairExample
    print $ take 10 $ parseHands dataStr
    print $ take 10 $ map (\(p1, p2) -> (handToRank p1, handToRank p2)) $ parseHands dataStr
    print $ take 10 $ map (\(r1, r2) -> match r1 r2) $ handsToRanks $ parseHands dataStr
    let test = take 10 $ filter (\(h1, h2) -> match (handToRank h1) (handToRank h2) == Win) $ parseHands dataStr
    print $ test
    print $ handsToRanks test
    let test2 = take 10 $ filter (\(h1, h2) -> (isOnePair h1) && (isOnePair h2)) $ parseHands dataStr
    print $ test2
    print $ handsToRanks test2


    print $ length $ filter (\(r1, r2) -> match r1 r2 == Win) $ handsToRanks $ parseHands dataStr

handsToRanks :: [(Hand, Hand)] -> [(Rank, Rank)]
handsToRanks hs = map (\(p1, p2) -> (handToRank p1, handToRank p2)) hs

data Result = Win | Loss | Draw deriving (Show, Eq)

match :: Rank -> Rank -> Result
match h1 h2
    | h1 > h2  = Win
    | h1 < h2  = Loss
    | h1 == h2 = Draw

handToRank :: Hand -> Rank
handToRank h
    | isRoyalFlush h    = RoyalFlush
    | isStraightFlush h = StraightFlush maxNumber
    | isFourOfAKind h   = FourOfAKind
    | isFullHouse h     = FullHouse
    | isFlush h         = Flush maxNumber
    | isStraight h      = Straight maxNumber
    | isThreeOfAKind h  = ThreeOfAKind maxNumber
    | isTwoPairs h      = TwoPairs maxNumber
    | isOnePair h       = OnePair (onePairNumber h) maxNumber
    | otherwise         = HighCard maxNumber
    where
        highNumber = maximum 
                  $ map snd
                  $ filter (\(count, _) -> count == 1) 
                  $ zip (map length $ countOfSameNumber h) $ (map number h)
        maxNumber = (number $ maximumBy (\c1 c2 -> compare (number c1) (number c2)) h)

isTwoPairs hand = (length $ filter (2 ==) $ map length $ countOfSameNumber hand) == 4

isStraight hand = isConsecutiveNumbers hand

isFlush hand = isAllSuitsSame hand

isOnePair hand = hasSameCardsCount hand 2
onePairNumber hand = head . (map snd) . filter (\(len, _) -> len == 2) $ zip (map length $ countOfSameNumber hand) (map number hand)

isThreeOfAKind hand = hasSameCardsCount hand 3

isFullHouse hand = isOnePair hand 
                && isThreeOfAKind hand

isFourOfAKind :: Hand -> Bool
isFourOfAKind hand = hasSameCardsCount hand 4


hasSameCardsCount hand n = any (n ==) $ map length $ countOfSameNumber hand
countOfSameNumber hand = map (\c1 -> filter (\c2 -> (number c2) == (number c1)) hand) hand


isStraightFlush :: Hand -> Bool
isStraightFlush hand = isAllSuitsSame hand && isConsecutiveNumbers hand

isRoyalFlush :: Hand -> Bool
isRoyalFlush hand = isAllSuitsSame hand
                 && isConsecutiveNumbers hand 
                 && ((minimum $ map number hand) == 10)

isConsecutiveNumbers :: Hand -> Bool
isConsecutiveNumbers hand = (sort $ map number hand) == [minNumber..minNumber+4]
    where
        minNumber = minimum $ map number hand

isAllSuitsSame :: Hand -> Bool
isAllSuitsSame hand = all (\c -> suit c == suit (hand !! 0)) $ drop 1 hand

data Rank =   HighCard Int -- high card
            | OnePair Int Int -- number of pair cards, high card
            | TwoPairs Int -- high card
            | ThreeOfAKind Int -- high card
            | Straight Int
            | Flush Int
            | FullHouse
            | FourOfAKind 
            | StraightFlush Int
            | RoyalFlush
            deriving (Eq, Ord, Show)

parseHands :: String -> [(Hand, Hand)]
parseHands str = map parseHand $ lines str

parseHand :: String -> (Hand, Hand)
parseHand str = splitAt 5 $ map readCard $ words str

data Card = Card Int Suit
    deriving (Eq, Ord)

suit :: Card -> Suit
suit (Card _ s) = s

number :: Card -> Int
number (Card n _) = n

type Cards = [Card]

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

