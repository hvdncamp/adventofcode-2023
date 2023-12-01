module Day01b where
import Data.Char (isDigit)
import GHC.Base (ord)

antwoord :: IO ()
antwoord = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day01.txt";
    print $ sum $ map (calibrationValue . filter isDigit . makeDigits) (lines input);

calibrationValue :: String -> Int
calibrationValue numbers = 10 * (ord (head numbers) - 48) + (ord (last numbers) - 48)

makeDigits:: String -> String
makeDigits ('o':'n':'e': rest)          = '1': makeDigits ('n':'e':rest)
makeDigits ('t':'w':'o': rest)          = '2': makeDigits ('w':'o':rest)
makeDigits ('t':'h':'r':'e':'e': rest)  = '3': makeDigits ('h':'r':'e':'e':rest)
makeDigits ('f':'o':'u':'r': rest)      = '4': makeDigits ('o':'u':'r':rest)
makeDigits ('f':'i':'v':'e': rest)      = '5': makeDigits ('i':'v':'e':rest)
makeDigits ('s':'i':'x': rest)          = '6': makeDigits ('i':'x':rest)
makeDigits ('s':'e':'v':'e':'n': rest)  = '7': makeDigits ('e':'v':'e':'n':rest)
makeDigits ('e':'i':'g':'h':'t': rest)  = '8': makeDigits ('i':'g':'h':'t':rest)
makeDigits ('n':'i':'n':'e': rest)      = '9': makeDigits ('i':'n':'e':rest)
makeDigits (a:rest)                     =  a : makeDigits rest
makeDigits ""                           = ""









