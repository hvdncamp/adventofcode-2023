module Day10a where
import Data.List (delete)

data Direction = N | E | S | W deriving (Show,Eq)
data Coordinate = Coordinate {x :: Int, y::Int} deriving (Eq)
newtype Pipe = Pipe {dirs :: [Direction]}
data PipeGrid = PipeGrid {pipes :: [[Pipe]], height :: Int, width :: Int, creature:: Coordinate}

---------------parsing-------------------------------------------------

readGrid :: [String] -> PipeGrid
readGrid rows = PipeGrid (map (map readPipe) rows) (length rows) (length $ head rows) (findCreature rows)
    where readPipe 'F' = Pipe [E,S]
          readPipe '|' = Pipe [N,S]
          readPipe '-' = Pipe [E,W]
          readPipe 'L' = Pipe [N,E]
          readPipe 'J' = Pipe [N,W]
          readPipe '7' = Pipe [S,W]
          readPipe '.' = Pipe []
          readPipe 'S' = Pipe [N,E,S,W]

findCreature :: [String] -> Coordinate
findCreature grid = let (s,x,y) = head $ filter (\(a,b,c) -> a == 'S') (indexPipes grid) in Coordinate x y
    where  mapFunction y line = [(c,x,y) | (x,c) <-zip [0..] line]
           indexPipes :: [String] -> [(Char, Int, Int)]
           indexPipes pipes = concat $ zipWith mapFunction [0..] pipes

---------------------solution--------------------------------------

cycleLength :: PipeGrid -> Int
cycleLength grid = let chosenDirection = head $ filter (canMoveTo grid (creature grid)) [N,E,S,W]  
    in 1 + distanceTo grid (creature grid) (move (creature grid) chosenDirection) (inverse chosenDirection) 

canMoveTo :: PipeGrid -> Coordinate -> Direction -> Bool
canMoveTo grid (Coordinate x y) dir = let newCoo = move (Coordinate x y) dir 
    in inGrid newCoo grid && (inverse dir `elem` dirs (pipeAt grid newCoo))

distanceTo :: PipeGrid -> Coordinate -> Coordinate -> Direction -> Int
distanceTo grid goal position fromDir
    |goal == position = 0
    |otherwise        = 1 + distanceTo grid goal (move position nextDir) (inverse nextDir)
        where nextDir = head $ delete fromDir $ dirs $ pipeAt grid position

pipeAt :: PipeGrid -> Coordinate -> Pipe
pipeAt grid (Coordinate x y) = (pipes grid!!y)!!x

move :: Coordinate -> Direction -> Coordinate
move (Coordinate x y) N = Coordinate  x   (y-1)
move (Coordinate x y) E = Coordinate (x+1) y
move (Coordinate x y) S = Coordinate  x   (y+1)
move (Coordinate x y) W = Coordinate (x-1) y

inverse :: Direction -> Direction
inverse E = W
inverse W = E
inverse N = S
inverse S = N

inGrid :: Coordinate -> PipeGrid -> Bool
inGrid (Coordinate x y) (PipeGrid _ h w _) = (x < w) && (y < h) && (x >= 0) && (y >= 0)

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day10.txt";
    print $ cycleLength (readGrid $ lines input ) `div` 2
