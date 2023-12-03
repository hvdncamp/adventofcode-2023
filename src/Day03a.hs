module Day03a where

import Data.Char (isDigit)
import GHC.Base (ord)

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day03.txt";
    print $ sum $ partNumbers $ lines input -- sum

partNumbers :: [String] -> [Int]
partNumbers lineList = traverseGrid lineList [] $ indexCharacters lineList

traverseGrid :: [String] -> [Int] -> [(Char,Int,Int)] -> [Int]
traverseGrid grid results ((c,y,x):rest) 
    |isDigit c = let (result, resLength, topleft, bottomright) = readNumber grid (y,x) in 
        traverseGrid grid (ifThenElse (containsCharacter grid topleft bottomright) result 0 : results) (drop (resLength - 1) rest)
    |otherwise = traverseGrid grid results rest
traverseGrid grid results [] = results

readNumber :: [String] -> (Int,Int) -> (Int, Int, (Int,Int), (Int,Int)) -- output (read integer, length, topleft corner of search area, bottomright corner of search area)
readNumber grid (y,x) = let (value, size) = number (readChar ((grid!!y)!!x)) 1 x in 
                    (value
                    , size
                    , (ifThenElse (y > 0)          (y - 1) y, ifThenElse (x > 0)                    (x - 1)    x)
                    , (ifThenElse (y < height - 1) (y + 1) y, ifThenElse (x + size - 1 < width - 1) (x + size) x))
    where width = length (head grid)
          height = length grid
          number :: Int -> Int -> Int -> (Int, Int) 
          number currentNumber currentLength currentPosition 
            |currentPosition == width - 1               = (currentNumber, currentLength)
            |isDigit ((grid!!y)!!(currentPosition + 1)) = number (currentNumber * 10 + readChar ((grid!!y)!!(currentPosition + 1))) (currentLength + 1) (currentPosition + 1)
            |otherwise                                  = (currentNumber, currentLength)


ifThenElse :: Bool -> p -> p -> p
ifThenElse bool yes no 
    |bool     = yes
    |not bool = no

readChar :: Char -> Int
readChar c = ord c - 48
    
containsCharacter :: [String] -> (Int,Int) -> (Int,Int) -> Bool
containsCharacter grid (ytop,xtop) (ybottom,xbottom) = any hasCharacter [(y,x)| y <- [ytop..ybottom], x <- [xtop..xbottom]]
    where hasCharacter (y,x)
            |isDigit ((grid!!y)!!x) = False
            |(grid!!y)!!x == '.'    = False
            |otherwise              = True

indexCharacters :: [String] -> [(Char, Int, Int)]
indexCharacters text = concat $ zipWith mapFunction [0..] text
    where mapFunction y line = [(c,y,x) | (x,c) <-zip [0..] line]








