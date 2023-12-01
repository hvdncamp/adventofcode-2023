module Main where

import qualified Day01a
import qualified Day01b
import qualified Day02a
import qualified Day02b
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
callProgram  a   b    = putStrLn $ "Day " ++ a ++ " number " ++ b ++ " has not been implemented (yet)"
