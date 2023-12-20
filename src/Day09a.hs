module Day09a where


nextNumber :: [Int] -> Int
nextNumber list = sum $ lastSequence list

lastSequence :: [Int] -> [Int] --returns the sequence of last numbers, from bottom to top
lastSequence list = next list []
    where next sequence lasts
            |all (0 ==) sequence = lasts
            |otherwise           = let (nextSeq, last) = diffSequence sequence in next nextSeq (last:lasts)

diffSequence :: [Int] -> ([Int], Int) --returns the new sequence and the last number of the previous one
diffSequence list = (zipWith (-) (tail list) list , last list)

-- Solution --------------------------------------------------------------------------


antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day09.txt";
    print $ sum $ map (nextNumber . map read . words) $ lines input  
