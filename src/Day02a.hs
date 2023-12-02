module Day02a where

import Data.List.Split

data Hand = Hand {red :: Int, green :: Int, blue :: Int}

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day02.txt";
    print $ sum $ map lineNumber $ filter possible $ lines input

lineNumber :: String -> Int
lineNumber = read . init . head . tail . words

removeGame :: String -> String
removeGame (':': rest) = tail rest
removeGame line        = removeGame $ tail line

possible :: String -> Bool
possible line = all possibleHand $ readLine line

possibleHand :: Hand -> Bool 
possibleHand (Hand r g b) = r <= 12 && g <= 13 && b <= 14

readLine :: String -> [Hand]
readLine line = map readHand $ splitOn "; " (removeGame line)

readHand :: String -> Hand
readHand text = foldl handFold (Hand {red =0,green=0,blue=0}) (splitOn ", " text)

handFold :: Hand -> String -> Hand
handFold (Hand r g b) string 
    | color == "red"    = Hand number g b
    | color == "green"  = Hand r number b
    | color == "blue"   = Hand r g number
    | otherwise         = Hand r g b
    where number = read $ head $ words string :: Int
          color  = last $ words string


