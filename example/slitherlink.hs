-- to generate cnf file of slitherlink(suriza rinku) puzzle -
-- 
--      o   o   o               o - o - o
--        3   2     solving     | 3   2 |
--      o   o   o    ==>>       o - o   o 
--            3                     | 3 |
--          o   o                   o - o
--
-- in dimacs format.


main = putStr "SlitherLink"
a = [x |x<- [1..4]]
a2 = [x^2 |x<- [1..4]]
v = [0..4]
mat = [[x |x<- [1..4]] |x<- [1..4]]
flatten = concat mat

-- idea: 
    --    1,4   3,4
    -- 0,3 o 2,3 o 4,3
    --    1,2   3,2
    -- 0,1 o 2,1 o 4,1
    --    1,0   3,0

-- basic rule(no numbers): 
--      each dot "o" has exactlly 0 or 2 nbd links. e.g { -o- },  {o} 
-- x_ ... encoded x ... encode(x)
-- x_ := x+1; y_ := y*5
-- (x,y)_ := (x+1)+y*5
enc x y = show $ (x+1)+y*5 -- = encode x y  = (x,y)_
int x = read x::Integer
-- \"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
-- = 2++3++4++5
generalRule x y = "\
    \-"++ enc (x-1) y ++" -"++ enc x (y+1) ++" -"++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
    
    \-"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" -"++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++" -"++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\

    \"++ enc (x-1) y ++" -"++ enc x (y+1) ++" -"++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
    \-"++ enc (x-1) y ++" "++ enc x (y+1) ++" -"++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
    \-"++ enc (x-1) y ++" -"++ enc x (y+1) ++" "++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
    \-"++ enc (x-1) y ++" -"++ enc x (y+1) ++" -"++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\

    \   -"++ enc x (y+1) ++" -"++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
    \-"++ enc (x-1) y ++"    -"++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
    \-"++ enc (x-1) y ++" -"++ enc x (y+1) ++"    -"++ enc x (y-1) ++" 0\n\
    \-"++ enc (x-1) y ++" -"++ enc x (y+1) ++" -"++ enc (x+1) y ++"   0\n"

field0 x y ="\
\-"++enc (x-1) y++" 0\n-"++enc x (y+1)++" 0\n-"++enc (x+1) y++" 0\n-"++enc x (y-1)++" 0\n"

rule12 = generalRule 1 2
printRule12 = putStr rule12


-- second basic rule: edges           (all set to False, i.e. no link)
-- (0,y) = neg - 
-- x=0,y=[1,3], ysplosno = [1,3,..n]
-- [x| y<-[1,3]]
setFalse x y = "-"++ enc x y ++" 0\n"
-- edge's values set to False (no link),
--  at {x=0 or x=4, y=odd } and at {y=0 or y=4, x=odd } :
edges_04 = unwords [setFalse 0 i++setFalse 4 i++setFalse i 0++setFalse i 4 | i<-[1,3]]
-- -- --  at y=0 and y=4, x=odd : OLD:
-- -- -- edges_y04 = unwords [setFalse x 0++setFalse x 4 | x<-[1,3]] -- OLD

sf = setFalse 0 1


-- write = writeFile "slither_min.txt" rule12
-- main = write

----------------  Example: smalest minnimum  (4 dots, solution 4 links=square) ------------------------------------------
--              o     o            o  -  o    
--                         ->      |     |
--              o     o            o  -  o

-- plan: set general rule to (x,y)\in {(1,1),(1,3),(3,1),(3,3)}
ruleExample = unwords [ unwords [ generalRule x y | x<- [1,3]] | y<- [1,3]]
firstLink = enc 1 2++" 0" -- to avoid empty solution
wholeRule = ruleExample ++ edges_04 ++ firstLink
writeExample = writeFile "slither_min2.txt" wholeRule

-- solution_min :
stringi =  "-20 -24 -18 -22 -10 -14 -8 -12 -4 -19 -16 -17 -2 -9 -6"
sol2 = "-18 -14 -8 -12 7 -24 -4 -20 -16 -22 -2 -10 -6 0"
sol3 =   "18 14 8 12 -24 -4 -20 -16 -22 -2 -10 -6 0"
negStr s = unwords $ map (\d -> show $ abs $ int d) $ words s 
poz = negStr sol2
-- decode solution:
decodeSol f l = map ( \xy -> "("++(show $ decodex $ f $ int xy )++","++(show $ decodey $ f $ int xy )++")") $ words $ init l 
decode2 s = map ( \xy -> ( decodex $ int xy , decodey $ int xy )) $ words s 
decodeLine l = map ( \xy -> ( decodex $ int xy , decodey $ int xy )) $ words $ init l 

    -- map ( \xy -> show $ decodex $ int xy++"," ) $ words s 
--  (x+1) + y*5
-- decode n = (  ((n-1) `mod` 5), (floor $ (n-1)/5) )
decodex n = (n-1) `rem` 5
decodey n = (n-1) `div` 5
-- decodeFile 
refi fileName = do
        r <- readFile fileName
        let ls = lines r
        -- putStr $ show ls -- works
        -- let lista = map (\l -> decodeLine l ) ls
        let list = map (\l -> unwords $ decodeSol abs l ) ls 
        let file = unlines list
        putStr  file
        -- putStr $ show list
        putStr "last expression"
        -- putStr r

slithFile = readFile "slither_min.txt"
refislith = refi "slither_min.txt"
-- words slithFile