-- this file is the kernel of SAT solver, i.e. the dpll algorithm
module Dpll where
import FindPure -- has preparatory thing for dpll

-- ----- plan = theory------
-- -- dpll:
-- -- 1.Repeat:                                                 = 1. step
-- --         * Find any _unit clause_ (l) in \phi      -- look FindPure.hs
-- --                 or _pure literal_ l in \phi       -- look FindPure.hs
-- --         * \phi:= Simplify \phi by l 
-- --                 val:= [val,l]
-- --         until there are no more unit clauses
-- --         or pure literals.


------ simplify, prepare things: ------------------
-- l= literal
hasLiteral L l = False
hasLiteral (V h t) l = if h == l then True else hasLiteral t l
omit l L = L
omit l (V h t) = if h==l then omit l t else V h (omit l t)
neg ('-':w) = w 
neg l = '-':l
simplify l T = T
simplify l (A h t) = if h `hasLiteral` l 
                then simplify l t 
                else A (omit (neg l) h) (simplify l t)
------ EndOf simplify. ------------------

-- redox reduces/simplifies formula by repeating. = step 1. of dpll ||  redox = repeat 
redox (formula, val) = let foundUnit = findUnit formula
                in case foundUnit of
                        [ ] -> let foundPureLit = findPureLiteral formula
                                in  case foundPureLit of 
                                        [] -> (formula, val)
                                        [elt]  -> redox (simplify elt formula, elt:val)
                        [elt]  -> redox (simplify elt formula, elt:val)
-- -- EndOf redox

---- pre dpll -------------
firstLiteral (A (V h _) _) = h      -- chooses first literal "h" that it sees.
-- elemL konj = ... checks if () in clause => then fail. 
elemL T = False
elemL (A L t) = True
elemL (A _ t) = elemL t

-- Input: dpll takes phi = cnf variable of type Konj, defined in FindPure.hs
-- Output: returns satfisfying valuation of False answer.
dpll (phi, val) = case (redox (phi, val)) of  -- step 1.repeat
        (T, satval) -> (True, satval)               -- step 2. []
        (phir, valr) -> if elemL phir then (False, []) -- step 2. ()
                        else let l = firstLiteral phir            -- step 3. choose
                        in case (dpll (A (V l L) phir, valr)) of
                                (True, sat) -> (True, sat)
                                (False, _) -> dpll (A (V (neg l) L) phir, valr)
