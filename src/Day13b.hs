module Day13b where

import Data.List.Split ( splitOn )

data Alignment = Row | Column

canFoldAt :: [String] -> (Int, Alignment)
canFoldAt input = head ([(i, Row) | i<-[1..length input - 1], 1 == diffWhenFoldAtRow input i] ++ [(i, Column) | i<-[1.. length (head input) - 1], 1 == diffWhenFoldAtColumn input i] )

diffWhenFoldAtColumn :: [String] -> Int -> Int
diffWhenFoldAtColumn input i = sum $ map diffOnLine input
    where diffOnLine :: String -> Int
          diffOnLine line = sum $ zipWith (\a b -> if a == b then 0 else 1) (reverse $ take i line) (drop i line)

diffWhenFoldAtRow :: [String] -> Int -> Int
diffWhenFoldAtRow input i = sum $ zipWith diffBetweenLines (reverse $ take i input) (drop i input)
    where diffBetweenLines line1 line2 = sum $ zipWith (\a b -> if a == b then 0 else 1) line1 line2

score :: (Int, Alignment) -> Int
score (n, Row)    = 100 * n
score (n, Column) = n

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day13.txt";
    print $ sum $ map (score . canFoldAt) $ splitOn [""] $ lines input
