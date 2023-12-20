module Day09b where

nextNumber :: [Int] -> Int
nextNumber list = foldl foldFirsts 0 $ lastSequence list 
    where foldFirsts x a = a - x

lastSequence :: [Int] -> [Int] --returns the sequence of first numbers, from bottom to top
lastSequence list = next list []
    where next sequence firsts
            |all (0 ==) sequence = firsts
            |otherwise           = let (nextSeq, first) = diffSequence sequence in next nextSeq (first:firsts)

diffSequence :: [Int] -> ([Int], Int) --returns the new sequence and the last number of the previous one
diffSequence list = (zipWith (-) (tail list) list , head list)

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day09.txt";
    print $ sum $ map (nextNumber . map read . words) $ lines input  
