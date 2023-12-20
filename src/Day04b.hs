module Day04b where

import Data.List.Split

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day04.txt";
    let inputLines = lines input in
        print $ sum $ foldl foldCards [1 | _<-inputLines] $ zip [1..] (map (map (map read . words) . splitOn "|" . drop 10) inputLines)

calculateScore :: [[Int]] -> Int
calculateScore (winning : actual : emptyRest) = sum $ map (\a -> if a `elem` actual then 1 else 0) winning
calculateScore other                          = 0 -- should not be possible

foldCards :: [Int] -> (Int,[[Int]]) -> [Int]
foldCards counts (number,newCard) = zipWith (\n i -> n + if i > number && i <= number + calculateScore newCard then counts!!(number - 1) else 0) counts [1..]
