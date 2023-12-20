module Day15b where

import Data.List.Split ( splitOn )
import GHC.Base (ord)

data Action = Action {name::String, atype :: ActionType, number :: Int}
data Lens = Lens {label::String, focalLength :: Int} deriving (Show, Eq)
data ActionType = Remove | Add | None deriving (Eq)
data Box = Box {boxNumber :: Int, lenses :: [Lens]} deriving (Show)

readAction :: String -> Action
readAction = foldl foldFunction (Action "" None 0)
    where foldFunction (Action lab typ nr) next  
            | next == '-' = Action lab Remove 0
            | next == '=' = Action lab Add    0
            | typ == Add  = Action lab Add (nr * 10 + (ord next - 48))
            | otherwise   = Action (lab ++ [next]) None 0

doAction :: [Box] -> Action -> [Box]
doAction boxes (Action lab typ nr) 
    |typ == Remove = map removeMap boxes
    |typ == Add    = map addMap boxes
    where removeMap (Box n ls)
            |n == score lab = Box n $ filter (\lens -> label lens /= lab) ls
            |otherwise      = Box n ls
          addMap (Box n ls)
            |n == score lab = if any (\l -> label l == lab) ls then Box n (map (\l -> if label l == lab then Lens lab nr else l) ls) else Box n (ls ++ [Lens lab nr])
            |otherwise      = Box n ls

score :: String -> Int
score = foldl foldFunction 0 
    where foldFunction n next  = ((n + ord next) * 17) `mod` 256

focusingPower :: Box -> Int
focusingPower (Box nr ls) = (nr + 1) * sum (zipWith (\i l -> i * focalLength l) [1..] ls)

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day15.txt";
    print $ sum $ map focusingPower $ foldl doAction [Box i [] | i<-[0..255]] $ map readAction $ splitOn "," $ init input
