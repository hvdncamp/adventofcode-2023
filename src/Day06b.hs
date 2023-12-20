module Day06b where

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day06.txt";
    let x = map (read . concat . tail . words) $ lines input in print $ waysToWin (head x) (x!!1)

waysToWin :: Int -> Int -> Int
waysToWin time record = let d = time ^ 2 - 4 * record
        in 1 + ceiling (-1 + (fromIntegral time + sqrt (fromIntegral d)) / 2) - floor (1 + (fromIntegral time - sqrt (fromIntegral d)) / 2)