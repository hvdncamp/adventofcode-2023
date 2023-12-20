module Day07a where
import Data.List (sort, group,sortBy)

data Hand = Hand {cards :: [Int], bid :: Int}
data HandType = High | Pair | TwoPair | Three | FullHouse | Four | Five deriving (Eq,Ord,Show)

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day07.txt";
    print $ sum $ zipWith (\a b -> a * bid b) [1..] $ sortBy compareHands $ map (readHand . words) $ lines input 

readHand :: [String] -> Hand
readHand [cards, bid] = Hand {cards = map cardToNumber cards, bid = read bid}
readHand _ = Hand {cards = [], bid = 0}

cardToNumber :: Char -> Int
cardToNumber 'T' = 10
cardToNumber 'J' = 11
cardToNumber 'Q' = 12
cardToNumber 'K' = 13
cardToNumber 'A' = 14
cardToNumber  x  = read [x]

compareHands :: Hand -> Hand -> Ordering
compareHands (Hand cards1 _) (Hand cards2 _) = compare (handType cards1, cards1) (handType cards2, cards2)

handType :: [Int] -> HandType
handType cards =  case sort $ map length (group (sort cards)) of
                        [5] -> Five
                        [1,4] -> Four
                        [2,3] -> FullHouse
                        [1,1,3] -> Three
                        [1,2,2] -> TwoPair
                        [1,1,1,2] -> Pair
                        _ -> High