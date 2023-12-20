module Day14a where


rollNorthSum :: [String] -> Int
rollNorthSum input = sum $ zipWith rollNorthSumLine input [0..]
    where maxy = length input
          rollNorthSumLine line index = sum $ map (rockValue index) [0..(length line - 1)]
          rockValue y x 
                |(input!!y)!!x == 'O' = maxy - y + countEmptyUntilRock x (y - 1)
                |otherwise = 0
          countEmptyUntilRock x y 
                |y == -1              = 0
                |(input!!y)!!x == '#' = 0
                |(input!!y)!!x == 'O' = countEmptyUntilRock x (y-1)
                |(input!!y)!!x == '.' = 1 + countEmptyUntilRock x (y-1)


antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day14.txt";
    print $ rollNorthSum $ lines input
