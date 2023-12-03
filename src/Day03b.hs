module Day03b where

import Data.Char (isDigit)
import GHC.Base (ord)
import Data.List (groupBy, sort)

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day03.txt";
    print $ sum $ partNumbers $ lines input 

partNumbers :: [String] -> [Int]
partNumbers lineList = calculateGearNumbers [] $ sort $ traverseGrid lineList [] $ indexCharacters lineList

calculateGearNumbers :: [Int] -> [(Int,Int,Int)] -> [Int]
calculateGearNumbers res list = map (\((_,_,n):(_,_,n2) : rest) -> n * n2) 
                                $ filter (\list -> length list == 2) 
                                $ groupBy (\(a,b,c) (a2,b2,c2) -> a == a2 && b == b2) list

traverseGrid :: [String] -> [(Int,Int,Int)] -> [(Char,Int,Int)] -> [(Int,Int,Int)]
traverseGrid grid coordinates []             = coordinates
traverseGrid grid coordinates ((c,y,x):rest) 
    |isDigit c = let (result, resLength, topleft, bottomright) = readNumber grid (y,x) in 
        traverseGrid grid (map (\(a,b) -> (a,b,result)) (gearCoordinates grid topleft bottomright) ++ coordinates) (drop (resLength - 1) rest)
    |otherwise = traverseGrid grid coordinates rest

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

gearCoordinates :: [String] -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
gearCoordinates grid (ytop,xtop) (ybottom,xbottom) = concatMap gear [(y,x)| y <- [ytop..ybottom], x <- [xtop..xbottom]]
    where gear (y,x)
            |(grid!!y)!!x == '*' = [(y,x)]
            |otherwise           = []

ifThenElse :: Bool -> p -> p -> p
ifThenElse bool yes no 
    |bool     = yes
    |not bool = no

readChar :: Char -> Int
readChar c = ord c - 48

indexCharacters :: [String] -> [(Char, Int, Int)]
indexCharacters text = concat $ zipWith mapFunction [0..] text
    where mapFunction y line = [(c,y,x) | (x,c) <-zip [0..] line]








