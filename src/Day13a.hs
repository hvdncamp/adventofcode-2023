module Day13a where

import Data.List.Split ( splitOn )

data Alignment = Row | Column

canFoldAt :: [String] -> (Int, Alignment)
canFoldAt input = head ([(i, Row) | i<-[1..length input - 1], canFoldAtRow input i] ++ [(i, Column) | i<-[1.. length (head input) - 1], canFoldAtColumn input i] )

canFoldAtColumn :: [String] -> Int -> Bool
canFoldAtColumn input i = all canFoldLine input
    where canFoldLine :: String -> Bool
          canFoldLine line = and $ zipWith (==) (reverse $ take i line) (drop i line)

canFoldAtRow :: [String] -> Int -> Bool
canFoldAtRow input i = and $ zipWith (==) (reverse $ take i input) (drop i input)

score :: (Int, Alignment) -> Int
score (n, Row)    = 100 * n
score (n, Column) = n 

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day13.txt";
    print $ sum $ map (score . canFoldAt) $ splitOn [""] $ lines input 
