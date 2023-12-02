module Day02b where

import Data.List.Split ( splitOn )

data Hand = Hand {red :: Int, green :: Int, blue :: Int}

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day02.txt";
    print $ sum $ map (power . maxHand . readLine) $ lines input

power :: Hand -> Int
power (Hand r g b) = r * g * b

maxHand :: [Hand] -> Hand
maxHand = foldl maxFold (Hand {red =0,green=0,blue=0})

maxFold :: Hand -> Hand -> Hand
maxFold (Hand maxr maxg maxb) (Hand r g b) = Hand (max maxr r) (max maxg g) (max maxb b)

readLine :: String -> [Hand]
readLine line = map readHand $ splitOn "; " (removeGame line)

removeGame :: String -> String
removeGame (':': rest) = tail rest
removeGame line        = removeGame $ tail line

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


