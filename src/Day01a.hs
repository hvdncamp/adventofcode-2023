module Day01a where
import Data.Char (isDigit)
import GHC.Base (ord)

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day01.txt";
    print $ sum $ map (calibrationValue . filter isDigit) (lines input);

calibrationValue :: String -> Int
calibrationValue numbers = 10 * (ord (head numbers) - 48) + (ord (last numbers) - 48)