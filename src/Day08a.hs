module Day08a where

import Data.Map hiding (drop)
import Text.Parsec 
import Text.Parsec.Token (GenTokenParser(whiteSpace))

data Node = Node {name :: String, left :: String, right :: String}
data Direction = L | R deriving (Show)

--Parsing-----------------------------------------------------------------

directionParser :: Parsec String st Direction
directionParser = dir <$> oneOf ['R','L']
        where dir 'R' = R
              dir 'L' = L

nodeNameParser:: Parsec String st String
nodeNameParser = many (oneOf ['A'..'Z']);

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

stepsFromTo :: Map String Node -> String -> String -> [Direction] -> Int
stepsFromTo _ _ _ [] = undefined -- should not happen, will only be called for infinite list
stepsFromTo nodeFromName start end (firstDirection:rest) 
    |start == end = 0
    |otherwise    = 1 + stepsFromTo nodeFromName (step firstDirection) end rest
    where step L = left  (nodeFromName ! start)
          step R = right (nodeFromName ! start)

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day08.txt";
    print (
        do
            directions <- parse (many directionParser) "" input
            nodes <- parse (many nodeParser) "" $ concat $ drop 2 (lines input)
            Right $ stepsFromTo (nodeMap nodes) "AAA" "ZZZ" (cycle directions)
        )



