module Dpll where
-- import qualified Data.Set as Set
import FindPure
import FindPure (findUnit, findPureLiteral, formula, formulaorig)


hello2 = putStrLn "SAT solving!"
-- main = hello2


-- ----- plan ------
-- -- dpll:
-- -- 1.Repeat:
-- --         * Find any _unit clause_ (l) in \phi
-- --                 or _pure literal_ l in \phi
-- --         * \phi:= Simplify \phi by l 
-- --                 val:= [val,l]
-- --         until there are no more unit clauses
-- --         or pure literals.


------ simplify, prepare things: ------------------
hasLiteral L l = False
hasLiteral (V h t) l = if h == l then True else hasLiteral t l
omit l L = L
omit l (V h t) = if h==l then omit l t else V h (omit l t)
neg ('-':w) = w 
neg l = '-':l
-- l= literal
simplify l T = T
simplify l (A h t) = if h `hasLiteral` l 
                then simplify l t 
                else A (omit (neg l) h) (simplify l t)
------ EndOf simplify. ------------------

-- redox = repeat , reapit= repeat = redo = reduce = redux = redox
redox (formula, val) = let foundUnit = findUnit formula
                in case foundUnit of
                        [ ] -> let foundPureLit = findPureLiteral formula
                                in  case foundPureLit of 
                                        [] -> (formula, val)
                                        [elt]  -> redox (simplify elt formula, elt:val)
                        [elt]  -> redox (simplify elt formula, elt:val)
-- test1 = redox (formula, [])
-- test_formulae = map (\x -> redox (x, [])) formulae
-- -- EndOf redox

---- -- pre dpll -------------
firstLiteral (A (V h _) _) = h 
-- test_first = firstLiteral formula
-- test_formulafirst = [pqner, uc, formula, formulaorig]
-- test_firstLiteral = map firstLiteral formulafirst
-- elemL konj = ... checks if () in clause => then fail. 
elemL T = False
elemL (A L t) = True
elemL (A _ t) = elemL t
-- test_elemL = elemL unsat
-- test_elemL2 = elemL formula
-- test_elemL3 = elemL $ A L (A (V "3" L) (A (V "-2" (V "-3" L)) (A (V "2" (V "-3" L)) T)))
-- test_elemL4 = elemL $ A (V "1" L) (A (V "-1" L) (A (V "-1" (V "3" L)) (A L (A (V "-1" (V "2" (V "-3" L))) T))))

-- dpll (phi, val) = case (redox (phi, val)) of  -- step 1.repeat
--                 (T, satval) -> (True, satval)               -- step 2. []
--                 (A L T,_) -> (False, [])            -- step 2. ()
--                 (phir, valr) -> let l = firstLiteral phir            -- step 3. choose
--                         in case (dpll (A (V l L) phir, valr)) of
--                                 (True, sat) -> (True, sat)
--                                 (False, _) -> dpll (A (V (neg l) L) phir, valr)
--                                 -- sat -> sat

dpll (phi, val) = case (redox (phi, val)) of  -- step 1.repeat
        (T, satval) -> (True, satval)               -- step 2. []
        (A L T,_) -> (False, [])            -- step 2. ()
        (phir, valr) -> if elemL phir then (False, [])
                        else let l = firstLiteral phir            -- step 3. choose
                        in case (dpll (A (V l L) phir, valr)) of
                                (True, sat) -> (True, sat)
                                (False, _) -> dpll (A (V (neg l) L) phir, valr)
                                -- sat -> sat
rdpll phi = dpll (phi, [])      -- rundpll



test = dpll (formulaorig, [])
testi = map (\x -> dpll (x,[])) formulae

unsatcnfd = "1 0\n-1 0\n-1 3 0\n-2 -3 0\n-1 2 -3 0\n"
unsat = A (V "1" L ) T



-- quick tests:-- Dela prav:
-- -- -- -- ["-rrr","qqq","-ppp"]
-- -- -- -- [A (V "ppp" (V "qqq" L)) (A (V "-ppp" (V "rrr" L)) 
-- -- -- -- (A (V "-qqq" (V "-rrr" L)) (A (V "-ppp" (V "qqq" (V "-rrr" L))) T))),

-- -- -- -- p v q) A (-p v r) A ( -q v -r) A (V -p v q v -r)


-- -- -- -- ["-ppp","-rrr"]
-- -- -- -- -p v q A -r 
-- -- -- -- A (V "-ppp" (V "q" L)) (A (V "-rrr" L) T),
-- -- -- -- T,
-- -- -- -- A L T,
-- -- -- -- p
-- -- -- -- A (V "ppp" L) T,

-- -- -- -- -p q

-- -- -- -- p v q) A (-p v r) A (-p v q v -r)
-- -- -- -- A (V "ppp" (V "qqq" L)) (A (V "-ppp" (V "rrr" L)) 
-- -- -- -- (A (V "-ppp" (V "qqq" (V "-rrr" L))) T))]

