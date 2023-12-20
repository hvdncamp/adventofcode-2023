module Day14b where


rollNESW :: [String] -> [String]
rollNESW field = field --TODO

rollN :: [String] -> [String]
rollN field = foldr rollLine field [1..length field]
    where rollLine index acc = undefined --TODO
          

rollTo :: [String] -> Int -> Int -> Int -> Int -> [String]
rollTo field x1 y1 x2 y2 = undefined --TODO


antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day14.txt";
    print$ rollNESW $ lines input --TODO
