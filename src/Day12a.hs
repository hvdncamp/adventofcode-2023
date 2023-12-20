module Day12a where

import Data.List.Split ( splitOn )


---------------parsing-------------------------------------------------

readLine :: String -> (String, [Int])
readLine line = let parts = words line in (head parts, map read $ splitOn "," $ last parts)

---------------------solution--------------------------------------

numberOfSolutions :: (String, [Int]) -> Int
numberOfSolutions (springs, [])         = if '#' `elem` springs then 0 else 1
numberOfSolutions (springs, list) = foldr foldSprings 0 [0 .. (length springs)]
    where foldSprings index acc
            |index + head list > length springs                       = acc --not enough space
            |'.' `elem` drop index (take (index + head list) springs) = acc --first number cannot be placed at index
            |'#' `elem` take index springs                            = acc
            |index + head list /= length springs 
                && springs!!(index + head list) == '#'                = acc --no space after
            |otherwise                                                = acc + numberOfSolutions (drop (index + head list + 1) springs, tail list)

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day12.txt";
    print $ sum $ map (numberOfSolutions . readLine) $ lines input
