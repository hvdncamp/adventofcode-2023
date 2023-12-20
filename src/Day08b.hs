module Day08b where

import Data.Map hiding (drop,map,filter,foldr)
import Text.Parsec

data Node = Node {name :: String, left :: String, right :: String}
data Direction = L | R deriving (Show)

--Parsing-----------------------------------------------------------------

directionParser :: Parsec String st Direction
directionParser = dir <$> oneOf ['R','L']
        where dir 'R' = R
              dir 'L' = L

nodeNameParser:: Parsec String st String
nodeNameParser = many (oneOf $ ['A'..'Z'] ++ ['1','2']);

nodeParser :: Parsec String st Node
nodeParser = do
                name <- nodeNameParser;
                string " = (";
                left <- nodeNameParser;
                string ", ";
                right <- nodeNameParser;
                string ")";
                return $ Node name left right

-- Solution --------------------------------------------------------------------------

nodeMap :: [Node] -> Map String Node
nodeMap nodes = fromList $ zip (Prelude.map name nodes) nodes

lcmList :: Integral a => [a] -> a
lcmList = foldr lcm 1

stepsFrom :: Map String Node -> [String] -> [Direction] -> Int
stepsFrom nodeMap starts directions = lcmList $ map (stepsToZ nodeMap directions) starts

stepsToZ :: Map String Node -> [Direction] -> String -> Int
stepsToZ nodeFromName (firstDirection:rest) position
    |position!!2 == 'Z' = 0
    |otherwise          = 1 + stepsToZ nodeFromName rest (step firstDirection position) 
     where step L position = left  (nodeFromName ! position)
           step R position = right (nodeFromName ! position)

startPositions :: [String] -> [String]
startPositions = filter (\str -> str!!2 == 'A')

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day08.txt";
    print (
        do
            directions <- parse (many directionParser) "" input
            nodes <- parse (many nodeParser) "" $ concat $ drop 2 (lines input)
            Right $ stepsFrom (nodeMap nodes) (startPositions $ map name nodes) (cycle directions)
        )



