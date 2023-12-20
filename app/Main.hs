module Main where

import qualified Day01a
import qualified Day01b
import qualified Day02a
import qualified Day02b
import qualified Day03a
import qualified Day03b
import qualified Day04a
import qualified Day04b
import qualified Day05a
import qualified Day05b
import qualified Day06a
import qualified Day06b
import qualified Day07a
import qualified Day07b
import qualified Day08a
import qualified Day08b
import qualified Day09a
import qualified Day09b
import qualified Day10a
import qualified Day10b
import qualified Day11a
import qualified Day11b
import qualified Day12a
import qualified Day12b
import qualified Day13a
import qualified Day13b
import qualified Day14a
import qualified Day14b
import qualified Day15a
import qualified Day15b
import qualified Day16a
import qualified Day16b
import qualified Day17
import qualified Day19



import System.Environment

main :: IO ()
main = do
  [day, number] <- getArgs;
  callProgram day number;

callProgram :: String -> String -> IO ()
callProgram "1" "1"   = Day01a.antwoord
callProgram "1" "2"   = Day01b.antwoord
callProgram "2" "1"   = Day02a.antwoord
callProgram "2" "2"   = Day02b.antwoord
callProgram "3" "1"   = Day03a.antwoord
callProgram "3" "2"   = Day03b.antwoord
callProgram "4" "1"   = Day04a.antwoord
callProgram "4" "2"   = Day04b.antwoord
callProgram "5" "1"   = Day05a.antwoord
callProgram "5" "2"   = Day05b.antwoord
callProgram "6" "1"   = Day06a.antwoord
callProgram "6" "2"   = Day06b.antwoord
callProgram "7" "1"   = Day07a.antwoord
callProgram "7" "2"   = Day07b.antwoord
callProgram "8" "1"   = Day08a.antwoord
callProgram "8" "2"   = Day08b.antwoord
callProgram "9" "1"   = Day09a.antwoord
callProgram "9" "2"   = Day09b.antwoord
callProgram "10" "1"   = Day10a.antwoord
callProgram "10" "2"   = Day10b.antwoord
callProgram "11" "1"   = Day11a.antwoord
callProgram "11" "2"   = Day11b.antwoord
callProgram "12" "1"   = Day12a.antwoord
callProgram "12" "2"   = Day12b.antwoord
callProgram "13" "1"   = Day13a.antwoord
callProgram "13" "2"   = Day13b.antwoord
callProgram "14" "1"   = Day14a.antwoord
callProgram "14" "2"   = Day14b.antwoord
callProgram "15" "1"   = Day15a.antwoord
callProgram "15" "2"   = Day15b.antwoord
callProgram "16" "1"   = Day16a.antwoord
callProgram "16" "2"   = Day16b.antwoord
callProgram "17" "1"   = Day17.antwoord1
callProgram "17" "2"   = Day17.antwoord2
callProgram "19" "1"   = Day19.antwoord1
callProgram "19" "2"   = Day19.antwoord2
callProgram  a   b    = putStrLn $ "Day " ++ a ++ " number " ++ b ++ " has not been implemented (yet)"
