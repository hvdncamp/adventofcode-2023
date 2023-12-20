module Day11b where

data Coordinate = Coordinate {x :: Int, y::Int} deriving (Eq)
newtype Grid = Grid {spaces :: [[Bool]]}

---------------parsing-------------------------------------------------

readGrid :: [String] -> Grid
readGrid rows = Grid (map (map readCoo) rows)
    where readCoo '#' = True
          readCoo  _  = False

---------------------solution--------------------------------------

emptyLines :: [Coordinate] -> ([Int],[Int])
emptyLines galaxies = (filter (`notElem` xs) [0..maxX], filter (`notElem` ys) [0..maxY])
    where xs = map x galaxies
          ys = map y galaxies
          maxX = maximum xs
          maxY = maximum ys

findGalaxies :: Grid -> [Coordinate]
findGalaxies (Grid spaces) = map snd $ filter fst $ indexGrid spaces

indexGrid :: [[Bool]] -> [(Bool, Coordinate)]
indexGrid coos = concat $ zipWith mapFunction [0..] coos
    where mapFunction y line = [(c, Coordinate x y) | (x,c) <-zip [0..] line]


distance :: Coordinate -> Coordinate -> ([Int],[Int]) -> Int
distance (Coordinate x1 y1) (Coordinate x2 y2) (xs,ys) = sum [if xi `elem` xs then 1000000 else 1 | xi<-[(x1 + 1)..x2]] 
                                                       + sum [if yi `elem` ys then 1000000 else 1 | yi<-[(y1 + 1)..y2]]

totalDistance :: [Coordinate] -> Int
totalDistance galaxies = let empties = emptyLines galaxies in sum $ [distance g1 g2 empties | g1<-galaxies, g2<-galaxies]

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day11.txt";
    print $ totalDistance $ findGalaxies $ readGrid $ lines input
