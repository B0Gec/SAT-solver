-- this file is dealing with dimacs format. It is the executive program.
import FindPure
import Dpll 
import System.Environment -- for getArgs

-- Whole: textFile -> Konj cnf -> dpll -> solution.
-- this file (without importing) : textFile -> Konj cnf 


-- list of literals in a line of input file -> Disj disjunction
words2disj (x:xs) = V x (words2disj xs)
words2disj [] = L
disj line = words2disj $ init $ words line -- file line -> Disj disjunction

-- list of lines of clipped input file -> Konj cnf
lines2konj [] = T 
lines2konj (x:xs) = A (disj x) (lines2konj xs)
-- clippin :
cnfLines text = filter (\line -> not $ elem (head $ words line) ["c", "p"]) $ lines text
konj fileText = lines2konj $ cnfLines fileText -- file text -> Konj cnf

-- compose: (textFile -> Konj cnf) o (Konj cnf -> dpll -> solution) = textFile -> solution.
handOut fileText 
        | ans = unwords val
        | otherwise = "0"
        where (ans, val) = dpll (konj fileText, [])

-- doArgs does the job. I.e. executes read/write and uses dpll to solve it.
doArgs = do 
        [i,o] <- getArgs
        putStrLn "This is the SAT App!\n\nI am SAT solving right now, please wait ..."
        fileText <- readFile i
        let printOut = handOut fileText
        writeFile o printOut
        putStrLn $ "Solution on screen: \n"++printOut
        putStr "What you see is the solution.\nfading out...\n"
main = doArgs