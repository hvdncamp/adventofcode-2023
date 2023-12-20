module Day04a where

import Data.List.Split

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day04.txt";
    print $ sum $ map (calculateScore . map (map read . words) . splitOn "|" . drop 10) $ lines input 

calculateScore :: [[Int]] -> Int
calculateScore (winning : actual : emptyRest) = let score = sum (map (\a -> if a `elem` actual then 1 else 0) winning) in if score == 0 then 0 else 2^(score - 1)
calculateScore other                          = 0 -- should not be possible
