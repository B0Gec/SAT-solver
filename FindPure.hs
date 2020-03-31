module FindPure where
import qualified Data.Set as Set

-- main = hello
hello = putStrLn "SAT solving!"

-- cnf = [(p v q)A(-p v r)A(-q v -r)A(-p v q v -r)] ... 
-- A=and; V=or; -=not; T=Veritas; L=Bottom 
data Konj = T | A Disj Konj
        deriving Show
data Disj = L | V String Disj 
        deriving Show
-- -- -- Disj:  
porq =  ( V "ppp" (V "qqq" L) ) -- p v q .
vempty = L       -- (False)  -- -> [()] = False             
vuc = V "ppp" L    -- (p)
-- -- -- Unit = string: 
ner = "-rrr"     -- (-r)
-- -- -- Konj: 
pqner =  A (V "-ppp" (V "q" L) ) (A (V "-rrr" L ) T) -- (p v q)A(-r).
empty = ((T))      -- [ ] = true 
pfalse = A L T    -- [()] = false
uc = A (V "ppp" L ) T    -- [(p)]

formulae = [pqner, empty, pfalse, uc, formula, formulaorig]

-- formula = [(p v q)A(-p v r)A(-q v -r)A(-p v q v -r)]
formula = A (V "ppp" (V "qqq" L)) (A (V "-ppp" (V "rrr" L)) (A (V "-ppp" (V "qqq" (V "-rrr" L) )) T)  )   -- (p v q)A(-p v r)A(-p v q v -r)]
formulaorig = A (V "ppp" (V "qqq" L)) 
 (A (V "-ppp" (V "rrr" L)) 
 (A (V "-qqq" (V "-rrr" L)) 
 (A (V "-ppp" (V "qqq" (V "-rrr" L) )) T)  ) ) 
-- unit clause : [(p v q)A(-r)] =: A ( V p (V q L) ) (N r)
------------------EndOf Formula examples--------------------------------------



-- ----- plan ------
-- -- dpll:
-- -- 1.Repeat:
-- --         * Find any _unit clause_ (l) in \phi
-- --                 or _pure literal_ l in \phi
-- --         * \phi:= Simplify \phi by l 
-- --                 val:= [val,l]
-- --         until there are no more unit clauses
-- --         or pure literals.


------- findUnit= finds unit clause in \phi ---------------------------------------
findUnit T = []
-- findUnit A L T = [] -- ta primer se ujame z tretjim case-om
findUnit (A (V u L) t) = [u]      -- u = unit
findUnit (A h t) = findUnit t          
------- EndOf: findUnit ---------------------------------------


------- findPureLiteral= finds pure literal in \phi ---------------------------------------

-- listVars returns list of all (possibly repeated) (free) variables in the formula.
listVars b T = []
listVars b (A h t) = disjVars b h ++ listVars b t
disjVars b L = []
disjVars b (V s t) = unitVars b s : disjVars b t                     -- s = String
unitVars True w = w 
unitVars False ('-':t) = t
unitVars False t = t
-- test_freeVars = map (\x -> listVars False x) formulae

w_o list guy = filter (\x -> not (x == guy)) list -- w_o = w/o=with/out
-- finds Pure literal from given list:
findPureOnList  [] = []
findPureOnList (('-':w):t) = if (w `elem` t) 
                then findPureOnList (t `w_o` w) 
                else [('-':w)]
findPureOnList (h:t) = [h] -- to je primer ["p","q","r"] -> "p" je pure.
-- testp = findPureOnList (setSort units)
setSort list = Set.toAscList (Set.fromList list)        --sort list using Sets
findPureLiteral formula = findPureOnList (setSort (listVars True formula))
-- t1 = map findPureLiteral formulae

