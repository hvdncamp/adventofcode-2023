module Day16b where
import Data.List (nubBy)

data Direction = N | E | S | W deriving (Show,Eq)
data Coordinate = Coordinate {x :: Int, y::Int} deriving (Show,Eq)

---------------------solution--------------------------------------

followBeam :: [(Coordinate, Direction)] -> [String] -> Coordinate -> Direction -> [(Coordinate, Direction)]
followBeam done grid (Coordinate x y) dir 
    |not $ inGrid (Coordinate x y) grid = done
    |(Coordinate x y, dir) `elem` done  = done
    |otherwise                          = foldr foldDirs ((Coordinate x y, dir):done) $ nextDirs ((grid!!y)!!x) dir 
    where foldDirs :: Direction -> [(Coordinate, Direction)] -> [(Coordinate, Direction)]
          foldDirs d acc = followBeam acc grid (move (Coordinate x y) d) d


nextDirs :: Char -> Direction -> [Direction]
nextDirs '|'  N = [N]
nextDirs '|'  S = [S]
nextDirs '|'  d = [N,S]
nextDirs '-'  W = [W]
nextDirs '-'  E = [E]
nextDirs '-'  d = [E,W]
nextDirs '/'  E = [N]
nextDirs '/'  S = [W]
nextDirs '/'  W = [S]
nextDirs '/'  N = [E]
nextDirs '\\' E = [S]
nextDirs '\\' S = [E]
nextDirs '\\' W = [N]
nextDirs '\\' N = [W]
nextDirs '.'  d = [d]
nextDirs  _   _ = []

move :: Coordinate -> Direction -> Coordinate
move (Coordinate x y) N = Coordinate  x   (y-1)
move (Coordinate x y) E = Coordinate (x+1) y
move (Coordinate x y) S = Coordinate  x   (y+1)
move (Coordinate x y) W = Coordinate (x-1) y

inGrid :: Coordinate -> [String] -> Bool
inGrid (Coordinate x y) grid = let h = length grid in 
                               let w = length $ head grid 
                               in (x < w) && (y < h) && (x >= 0) && (y >= 0)

tilesEnergized :: [String] -> (Coordinate, Direction) -> Int
tilesEnergized grid (coo,dir) = length $ nubBy (\a b -> fst a == fst b) $ followBeam [] grid coo dir

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day16.txt";
    let height = length $ lines input in let width = length $ head $ lines input in 
        print $ maximum $ map (tilesEnergized (lines input)) $ [(Coordinate 0 i, E) | i<-[0..height - 1]] ++ [(Coordinate (width - 1) i, W) | i<-[0..height - 1]] 
                                                                ++ [(Coordinate i 0, S) | i<-[0..width - 1]] ++ [(Coordinate i (height - 1), N) | i<-[0..width - 1]]
