module Day15a where

import Data.List.Split ( splitOn )
import GHC.Base (ord)

score :: String -> Int
score = foldl foldFunction 0 
    where foldFunction n next  = ((n + ord next) * 17) `mod` 256

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day15.txt";
    print $ sum $ map score $ splitOn "," $ init input
