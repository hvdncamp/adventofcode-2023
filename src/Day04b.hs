module Day04b where

import Data.List.Split

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day04.txt";
    let inputLines = lines input in
        print $ sum $ foldl foldCards [1 | _<-inputLines] $ zip [1..] (map (map (map read . words) . splitOn "|" . drop 10) inputLines)

calculateScore :: [[Int]] -> Int
calculateScore (winning : actual : emptyRest) = sum $ map (\a -> ifThenElse (a `elem` actual) 1 0) winning
calculateScore other                          = 0 -- should not be possible

foldCards :: [Int] -> (Int,[[Int]]) -> [Int]
foldCards counts (number,newCard) = let numberOfCards = counts!!(number - 1) 
                                    in zipWith (\n i -> n + 
                                            ifThenElse (i > number && i <= number + calculateScore newCard) numberOfCards 0) counts [1..]

ifThenElse :: Bool -> p -> p -> p
ifThenElse bool yes no 
    |bool     = yes
    |not bool = no