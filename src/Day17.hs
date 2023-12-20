module Day17 where
import Data.List (nubBy)
import Data.Char (ord)
import Data.Function.Memoize (memoize, Memoizable)
import GHC.Base (maxInt)

data Direction = N | E | S | W deriving (Show,Eq)
data Coordinate = Coordinate {x :: Int, y::Int} deriving (Show,Eq)

---------------------parsing---------------------------------------

readGrid :: String -> [[Int]]
readGrid input = map (map (\c -> ord c - 48)) $ lines input

---------------------solution--------------------------------------

findPath :: ([[Int]], Coordinate, Direction) -> Int
findPath (grid, Coordinate x y, dir) 
    |not $ inGrid (Coordinate x y) grid                  = maxInt
    |y == length grid - 1 && x == length (head grid) - 1 = (grid!!y)!!x
    |otherwise                                           = let options = [(d, steps) | d <- [S,E,N,W], d /= dir, steps <- [1,2,3]] 
                                                        in let minDist = minimum $ map (\(d,steps) -> findPath (grid, move (Coordinate x y) d steps, d)) options
                                                        in ((grid!!y)!!x) + minDist 

move :: Coordinate -> Direction -> Int -> Coordinate
move (Coordinate x y) N n = Coordinate  x   (y-n)
move (Coordinate x y) E n = Coordinate (x+n) y
move (Coordinate x y) S n = Coordinate  x   (y+n)
move (Coordinate x y) W n = Coordinate (x-n) y

inGrid :: Coordinate -> [[Int]] -> Bool
inGrid (Coordinate x y) grid = let h = length grid in 
                               let w = length $ head grid 
                               in (x < w) && (y < h) && (x >= 0) && (y >= 0)

antwoord1 :: IO ()
antwoord1 = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/example17.txt";
    let grid = readGrid input in 
        --print $ memoize findPath (grid, Coordinate 0 0, E) - head (head grid)
        print "TODO"

antwoord2 :: IO ()
antwoord2 = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/example17.txt";
    let grid = readGrid input in 
        print "TODO"