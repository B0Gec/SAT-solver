-- this file has preparatory functions and types for dpll, 
-- e.g. findUnit and findPureLiteral and type Konj ( i.e. conjunctive normal form)
module FindPure where
import qualified Data.Set as Set -- for sorting and losing repeated literals

-- cnf form example = [(p v q)A(-p v r)A(-q v -r)A(-p v q v -r)] 
-- A=and; V=or; -=not; T=Veritas; L=Bottom 
data Konj = T | A Disj Konj
        deriving Show
data Disj = L | V String Disj 
        deriving Show


-- ----- plan ------
-- -- dpll:
-- -- 1.Repeat:
-- --         * Find any _unit clause_ (l) in \phi      -- this 2 lines are in this file.
-- --                 or _pure literal_ l in \phi       -- this 2 lines are in this file.
-- --         * \phi:= Simplify \phi by l 
-- --                 val:= [val,l]
-- --         until there are no more unit clauses
-- --         or pure literals.


------- findUnit= finds unit clause in \phi ---------------------------------------
findUnit T = []
findUnit (A (V u L) t) = [u]      -- u = unit
findUnit (A h t) = findUnit t          
------- EndOf: findUnit ---------------------------------------


------- findPureLiteral= finds pure literal in \phi ---------------------------------------

-- listVars returns list of all (possibly repeated) (free) variables in the formula.
listVars T = []
listVars (A h t) = disjVars h ++ listVars t
disjVars L = []
disjVars (V s t) = s : disjVars t                -- s = String

-- w_o returns list without the guy.
w_o list guy = filter (\x -> not (x == guy)) list -- w_o = w/o=with/out
-- finds Pure literal via deducing it from given list:
findPureOnList  [] = []
findPureOnList (('-':w):t) = if (w `elem` t) 
                then findPureOnList (t `w_o` w) 
                else [('-':w)]
findPureOnList (h:t) = [h] -- to je primer ["p","q","r"] -> "p" je pure.
setSort list = Set.toAscList (Set.fromList list)        --sorts list using Sets
findPureLiteral formula = findPureOnList (setSort (listVars formula))

