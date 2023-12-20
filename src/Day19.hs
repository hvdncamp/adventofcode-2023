module Day19 where

import Text.Parsec

data Workflow = Workflow {name :: String, rules :: [Rule]}
data Rule = Rule {condition :: Part -> Bool, number :: Int, typ :: (Char,Char), result :: Result}
data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show)
data Result = A | R | Flow String | NotYet deriving (Eq,Show)

--Parsing-----------------------------------------------------------------

workflowParser :: Parsec String st Workflow
workflowParser = do
                    name <- nameParser
                    string "{"
                    rules <- many (try normalruleParser)
                    lastRule <- lastruleParser
                    string "}\n"
                    return $ Workflow name (rules ++ [lastRule])


nameParser:: Parsec String st String
nameParser = many (oneOf  ['a'..'z']);

normalruleParser :: Parsec String st Rule
normalruleParser = do
                category <- oneOf ['a','x','m','s']
                sign <- oneOf ['<','>']
                number <- many digit
                string ":"
                result <- resultParser
                string ","
                return $ Rule (\part -> sig sign (cat category part) (read number)) (read number) (category,sign) result
            where cat 'a' = a
                  cat 'x' = x
                  cat 'm' = m
                  cat 's' = s
                  sig '<' = (<)
                  sig '>' = (>)

lastruleParser :: Parsec String st Rule
lastruleParser = do
                    Rule (const True) (-1) ('_','_') <$> resultParser

resultParser :: Parsec String st Result
resultParser =
    toResult <$> many (oneOf $ ['R','A'] ++ ['a'..'z'])
    where toResult "A" = A
          toResult "R" = R
          toResult str = Flow str

partParser :: Parsec String st Part
partParser = do
                string "{x="
                xval <- many digit
                string ",m="
                mval <- many digit
                string ",a="
                aval <- many digit
                string ",s="
                sval <- many digit
                string "}\n"
                return $ Part (read xval) (read mval) (read aval) (read sval)

inputParser :: Parsec String st ([Workflow], [Part])
inputParser = do
                flows <- many workflowParser
                string "\n"
                parts <- many partParser
                return (flows, parts)

-- Solution --------------------------------------------------------------------------

applyOneFlow :: Workflow -> Part -> Result
applyOneFlow flow part = foldl flowFold NotYet (rules flow)
    where flowFold NotYet rule = if condition rule part then result rule else NotYet
          flowFold res    rule = res

applyFlows :: [Workflow] -> String -> Part -> Result
applyFlows flows start part = todo $ applyOneFlow (getFlow flows start) part
    where todo A           = A
          todo R           = R
          todo (Flow next) = applyFlows flows next part

getFlow :: [Workflow] -> String -> Workflow
getFlow flows nam = head $ filter (\flow -> name flow == nam) flows

countAccepted :: [Workflow] -> Workflow -> ((Int,Int),(Int,Int),(Int,Int),(Int,Int)) -> Int
countAccepted flows (Workflow n rs) ranges
    |not $ reasonableRange ranges  = 0
    |null rs                       = 0
    |typ firstRule    == ('_','_') = countResult (result firstRule) ranges
    |snd (typ firstRule) == '<'    = countAccepted flows (Workflow n (tail rs)) (set1 (fst $ typ firstRule) ranges (number firstRule))
                                    + countResult (result firstRule) (set2 (fst $ typ firstRule) ranges (number firstRule - 1))
    |snd (typ firstRule) == '>'    = countAccepted flows (Workflow n (tail rs)) (set2 (fst $ typ firstRule) ranges (number firstRule))
                                    + countResult (result firstRule) (set1 (fst $ typ firstRule) ranges (number firstRule + 1))
    where countResult A          rangs = countRange rangs
          countResult R          rangs = 0
          countResult (Flow str) rangs = countAccepted flows (getFlow flows str) rangs
          set1 'x' ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) val = ((val,a2),(b1,b2),(c1,c2),(d1,d2))
          set1 'm' ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) val = ((a1,a2),(val,b2),(c1,c2),(d1,d2))
          set1 'a' ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) val = ((a1,a2),(b1,b2),(val,c2),(d1,d2))
          set1 's' ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) val = ((a1,a2),(b1,b2),(c1,c2),(val,d2))
          set2 'x' ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) val = ((a1,val),(b1,b2),(c1,c2),(d1,d2))
          set2 'm' ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) val = ((a1,a2),(b1,val),(c1,c2),(d1,d2))
          set2 'a' ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) val = ((a1,a2),(b1,b2),(c1,val),(d1,d2))
          set2 's' ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) val = ((a1,a2),(b1,b2),(c1,c2),(d1,val))
          firstRule = head rs


countRange :: ((Int,Int),(Int,Int),(Int,Int),(Int,Int)) -> Int
countRange ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) = (a2 - a1 + 1) * (b2 - b1 + 1) * (c2 - c1 + 1) * (d2 - d1 + 1)

reasonableRange :: ((Int,Int),(Int,Int),(Int,Int),(Int,Int)) -> Bool
reasonableRange ((a1,a2),(b1,b2),(c1,c2),(d1,d2)) = a2 >= a1 && b2 >= b1 && c2 >= c1 && d2 >= d1

antwoord1 :: IO ()
antwoord1 = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day19.txt";
    print (
        do
            (workFlows, parts) <- parse inputParser "" input
            Right $ sum $ map (\part -> x part + m part + a part + s part) $ filter (\part -> A == applyFlows workFlows "in" part) parts
        )

antwoord2 :: IO ()
antwoord2 = do
    input <- readFile "/home/heidi/personal/adventofcode2023/input/day19.txt";
    print (
        do
            (workFlows, _) <- parse inputParser "" input
            Right $ countAccepted workFlows (getFlow workFlows "in") ((1,4000),(1,4000),(1,4000),(1,4000))
        )
