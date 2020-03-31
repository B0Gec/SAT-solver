module Dimacs where
import FindPure
import Dpll 
import System.Environment

-- main = hello
hello = putStrLn "Dimacs -_-"

dimacstext = "c ...\nc...\np...\n1 23 4 0"
line1 = "1 23 4 -4 -455 34 5464 -234234 0"
preamble = "c -- formula in dimacs --\nc formula\ 
\= (p v q)A(-p v r)A(-q v -r)A(-p v q v -r)]\n\
\c Atom pxyn is represented by 16*(x-1)+4*(y-1)+n\
\\nc 11988 + 12+12 = 12012\nc 64 = # of variables;\ 
\453 = # of clauses (rows)\nc p cnf 64 453\nc\nc p q 0\n\
\c -p r 0\nc -q -r 0\nc -p q -r 0\nc\nc for real:\n\
\c 3 variables, 4 clauses\np cnf 3 4\n"
textcnf = "1 2 0\n-1 3 0\n-2 -3 0\n-1 2 -3 0\n"
text1 = preamble++textcnf
unsatcnf = "1 0\n-1 0\n-1 3 0\n-2 -3 0\n-1 2 -3 0\n"

words2disj (x:xs) = V x (words2disj xs)
words2disj [] = L
-- words2disj ["0"] = L
-- test_Words2disj = words2disj ["1", "23", "4"]
disj line = words2disj $ init $ words line
-- test_Line2disj = disj line1

lines2konj [] = T 
lines2konj (x:xs) = A (disj x) (lines2konj xs)
-- test_Lines2konj = lines2konj (lines textcnf) -- dela
cnfLines text = filter (\line -> not $ elem (head $ words line) ["c", "p"]) $ lines text
konj fileText = lines2konj $ cnfLines fileText

handOut fileText 
        | ans = unwords val
        | otherwise = "0"
        where (ans, val) = dpll (konj fileText, [])

doArgs = do 
        [i,o] <- getArgs
        putStrLn "This is the SAT App!"
        fileText <- readFile i
        --     write = writeFile "solution2.txt" $ handOut fileText
        let printOut = handOut fileText
        writeFile o printOut
        putStrLn $ "solution on screen: \n"++printOut
        putStr "fading out...\n"
-- main = hello
main = doArgs

