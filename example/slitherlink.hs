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
encode xdots ydots= encodeComp (2*xdots) (2*ydots)
encodeComp xmax ymax x y = show $ (x+1)+y*(xmax+1)  -- m n so maximalne koordinate: npr xmax = 4, ymax = 4 za simplest example
encSimplest = encode 4 4 -- simplest example
-- enc x y = show $ (x+1)+y*5 -- = encode x y  = (x,y)
int x = read x::Integer
-- \"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
-- = 2++3++4++5
generalRule enc x y = "\
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

field0 enc x y ="\
\-"++enc (x-1) y++" 0\n-"++enc x (y+1)++" 0\n-"++enc (x+1) y++" 0\n-"++enc x (y-1)++" 0\n"
field4 enc x y ="\
\"++enc (x-1) y++" 0\n"++enc x (y+1)++" 0\n"++enc (x+1) y++" 0\n"++enc x (y-1)++" 0\n"
fieldNot4 enc x y = "\
\-"++enc (x-1) y++" -"++enc x (y+1)++" -"++enc (x+1) y++" -"++enc x (y-1)++" 0\n"
-- field4 x y = mapFile negate $ field0 x y -- shortcut
field2 enc x y = "\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    
    \-"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" -"++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++" -"++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\

    \"++ enc (x-1) y ++" -"++ enc x (y+1) ++" -"++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
    \-"++ enc (x-1) y ++" "++ enc x (y+1) ++" -"++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
    \-"++ enc (x-1) y ++" -"++ enc x (y+1) ++" "++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
    \-"++ enc (x-1) y ++" -"++ enc x (y+1) ++" -"++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\

    \   "++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++"    "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++"    "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++"   0\n"
-- field3 x y =
    -- 2*(omega) all not ; and all yes.
    -- 4*(beta = (generalRule -) ) one not othr yes.
    -- 4*(delta = (field2 +) ) one empty othr yes.
    -- 6*(alpha) two yes two not.
    -- 6*(gamma) two empty two yes.
    -- 12*(-) one not one empty two yes.
field3 enc x y = "\
    \-"++ enc (x-1) y ++" -"++ enc x (y+1) ++" -"++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\

    \-"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" -"++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++" -"++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\

    \   "                 ++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++"    "                 ++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++"    "                 ++ enc x (y-1) ++" 0\n\
    \"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++             "   0\n\

\-"++ enc (x-1) y ++" -"++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
\-"++ enc (x-1) y ++" "++ enc x (y+1) ++" -"++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
\-"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
\"++ enc (x-1) y ++" -"++ enc x (y+1) ++" -"++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
\"++ enc (x-1) y ++" -"++ enc x (y+1) ++" "++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
\"++ enc (x-1) y ++" "++ enc x (y+1) ++" -"++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\

                                        \"++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
                    \"++ enc x (y+1) ++" "                    ++ enc x (y-1) ++" 0\n\
                    \"++ enc x (y+1) ++" "++ enc (x+1) y ++" "               ++ "0\n\
\"++ enc (x-1) y ++" "                                   ++ enc x (y-1) ++" 0\n\
\"++ enc (x-1) y ++" "               ++ enc (x+1) y ++" "               ++ "0\n\
\"++ enc (x-1) y ++" "++ enc x (y+1) ++" "                              ++ "0\n\



\-"++ enc (x-1) y ++" "                     ++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
\-"++ enc (x-1) y ++" "++ enc x (y+1) ++" "                     ++ enc x (y-1) ++" 0\n\
\-"++ enc (x-1) y ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" "                ++ "0\n\

\"              ++" -"++ enc x (y+1) ++" "++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
\"              ++" "++ enc x (y+1) ++" -"++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
\"              ++" "++ enc x (y+1) ++" "++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\

\"++ enc (x-1) y ++" -"++ enc x (y+1) ++" "                 ++ enc x (y-1) ++" 0\n\
\"++ enc (x-1) y ++" -"++ enc x (y+1) ++" "++ enc (x+1) y                  ++" 0\n\
\"++ enc (x-1) y                      ++" -"++ enc (x+1) y ++" "++ enc x (y-1) ++" 0\n\
\"++ enc (x-1) y ++" "                       ++ enc (x+1) y ++" -"++ enc x (y-1) ++" 0\n\
\"++ enc (x-1) y ++" "++ enc x (y+1) ++" -"++ enc (x+1) y ++                     " 0\n\
\"++ enc (x-1) y ++" "++ enc x (y+1) ++                     " -"++ enc x (y-1) ++" 0\n"
field1 enc x y = mapFile negate $ field3 enc x y
field enc n = case n of
    0 -> field0 enc 
    1 -> field1 enc
    2 -> field2 enc
    3 -> field3 enc
    4 -> field4 enc

-- second basic rule: edges           (all set to False, i.e. no link)
-- (0,y) = neg - 
-- x=0,y=[1,3], ysplosno = [1,3,..n]
-- [x| y<-[1,3]]
setFalse enc x y = "-"++ enc x y ++" 0\n"

-- main = write

----------------  Example: smalest minnimum  (4 dots, solution 4 links=square) ------------------------------------------
--              o     o            o  -  o    
--                          ->     |     |
--              o     o            o  -  o

-- edge's values set to False (no link),
--  at {x=0 or x=4, y=odd } and at {y=0 or y=4, x=odd } :
edges_04 = unwords [setFalse encSimplest 0 i++setFalse encSimplest 4 i++setFalse encSimplest i 0++setFalse encSimplest i 4 | i<-[1,3]]

-- plan: set general rule to (x,y)\in {(1,1),(1,3),(3,1),(3,3)}
generalRulSimpl = unwords [ unwords [ generalRule encSimplest x y | x<- [1,3]] | y<- [1,3]]
firstLink = encSimplest 1 2++" 0" -- to avoid empty solution
with4init = field4 encSimplest 2 2
wholeRule1 = generalRulSimpl ++ edges_04 ++ firstLink
wholeRule2 = generalRulSimpl ++ edges_04 ++ with4init
writeExample1 = writeFile "slither_min1.txt" wholeRule1
writeExample2 = writeFile "slither_min2.txt" wholeRule2
----------------  EndOf Example: smalest minnimum  (4 dots, solution 4 links=square) ------------------------------------------

-- solution_min :
stringi =  "-20 -24 -18 -22 -10 -14 -8 -12 -4 -19 -16 -17 -2 -9 -6"
sol2 = "-18 -14 -8 -12 7 -24 -4 -20 -16 -22 -2 -10 -6 0"
sol3 =   "18 14 8 12 -24 -4 -20 -16 -22 -2 -10 -6 0"
file1 = "18 14 8 12 -24 -4 -20 -16 -22 -2 -10 -6 0\n\
\18 14 8 12 -24 -4 -20 -16 -22 -2 -10 -6 0\n\
\18 14 8 12 -24 -4 -20 -16 -22 -2 -10 -6 0\n"
mapLine f l = unwords $ map (\d -> show $ f $ int d) $ words l
mapFile f text = unlines $ map (\line -> mapLine f line) $ lines text

-- absStr l = mapLine abs l
-- poz = absStr sol2
-- decode solution:
decodeSol enc f l = map ( \xy -> "("++(show $ decodex enc $ f $ int xy )++","++(show $ decodey enc $ f $ int xy )++")") $ words $ init l 
decode2 enc s = map ( \xy -> ( decodex enc $ int xy , decodey enc $ int xy )) $ words s 
decodeLine enc l = map ( \xy -> ( decodex enc $ int xy , decodey enc $ int xy )) $ words $ init l 
decodeFile enc text = unlines $ map (\l -> unwords $ decodeSol enc abs l ) $ lines text 
    -- map ( \xy -> show $ decodex $ int xy++"," ) $ words s 
--  (x+1) + y*5
-- decode n = (  ((n-1) `mod` 5), (floor $ (n-1)/5) )

decodex enc n = rem (n-1) $ (int ( enc 0 1))-1
decodey enc n = div (n-1) $ (int ( enc 0 1))-1

refi enc fileName = do
        r <- readFile fileName
        let ls = lines r
        -- putStr $ show ls -- works
        -- let lista = map (\l -> decodeLine l ) ls
        let list = map (\l -> unwords $ decodeSol enc abs l ) ls 
        let file = unlines list
        putStr  file
        -- putStr $ show list
        putStr "last expression"
        -- putStr r

slithFile = readFile "slither_min.txt"
refislith = refi (encode 2 2) "slither_min1.txt"
refi5x5 = refi (encode 6 6) "slither1_5x5.txt"
-- words slithFile



----------------  Example: rectangle shape (m*n dots) ---------------------
--  dots: 1,3,5,...,2*m-1 in x and 1,3,5,...,2*n-1 in y
-- x \in {0=edge,1=dot,2=field,...,m=dot,m+1=field,...2*m-1=dot,2*m = edge}
-- m = 5
-- n = 6 
edgesxSquare enc n = unwords [setFalse enc 0 y++setFalse enc (2*n) y | y<-[1,3..(2*n-1)]]
edgesySquare enc m = unwords [setFalse enc x 0++setFalse enc x (2*m) | x<-[1,3..(2*m-1)]]
generalSquare enc m n = unwords [ unwords [ generalRule enc x y | y<- [1,3..(2*n-1)] ] | x<- [1,3..(2*m-1)]]
-- in case not simplest example, i.e. not m=n=2, add probibited minimal square:
ruleNot4 enc m n = unwords [ unwords [ fieldNot4 enc x y | y<- [1..n] ] | x<- [1..m]]
generalSqNot4 enc m n =  generalSquare enc m n ++ ruleNot4 enc m n
-- specific numbers in certain field, where y and xfield is in human coordinates:
-- |   | 2 |  ... is second field, i.e. xfield = 2
setfield enc n xfield yfield = field enc n (2*xfield) (2*yfield)
templateSquare enc m n = edgesxSquare enc m ++ edgesySquare enc n ++ generalSquare enc m n
templateSqNot4 enc m n = edgesxSquare enc m ++ edgesySquare enc n ++ generalSqNot4 enc m n
-- exampleSquare = templateSquare 6 6 ++ setfield 
givenFields = [[1, 2, 1],[2, 3, 1]
    ,[3, 1, 2],[2, 5, 2]
    ,[3, 1, 3],[0, 2 ,3],[2, 3, 3],[1, 4, 3]
    ,[3,1,4]
    ,[2,2,5],[1,3,5],[2,4,5]]
rulesField =  unwords $ map (\e -> setfield (encode 6 6) (head e) (e !! 1)  (last e) ) givenFields
wholeSquare = templateSquare (encode 6 6) 6 6 ++ rulesField
wholeSqNot4 = templateSqNot4 (encode 6 6) 6 6 ++ rulesField

-- firstLinkSq = enco 1 2++" 0"
testgeneral = templateSqNot4 enco 6 6
writetg2 = writeFile "slither2.txt" testgeneral
writeSquare = writeFile "slither1_5x5.txt" wholeSqNot4
m=6
n=6
inli = [  [ (x, y) | y<- [1,3..(2*n-1)] ] | x<- [1,3..(2*m-1)]]
enco = encode 6 6
prit enc s = putStr $ decodeFile enc $ s
-- sol = "-126 140 152 -150 -138 -124 122 136 148 134 120 108 -106 50 64 76 62 56 42 -54 -20 34 46 -44 -32 -18 30 -100 -88 -74 86 72 -98 -70 -84 -96 80 94 68 -82 -168 -12 -166 -10 -164 -8 -162 -6 -160 -4 -158 -2 -156 -144 -130 -118 -104 -92 -78 -66 -52 -40 -26 -14"
slither2_txt_solution ="-142 -154 -128 -140 -116 -102 -114 -90 -76 -88 -64 -50 -62 -38 -24 -36 -152 -126 -138 -100 -112 -74 -86 -48 -60 -22 -34 -150 -124 -136 -98 -110 -72 -84 -46 -58 -20 -32 -148 -122 134 -96 108 -70 82 -44 56 -18 30 146 -120 132 -94 106 -68 80 -42 54 16 28 -168 -12 -166 -10 -164 -8 -162 -6 -160 -4 -158 -2 -156 -144 -130 -118 -104 -92 -78 -66 -52 -40 -26 -14"
-- sol = "64 76 -50 -62 38 24 -36 -90 102 88 -74 -48 60 116 142 154 -128 -114 140 22 -34 46 58 72 -100 -86 112 126 -152 -138 98 110 -124 -150 136 148 -134 -122 146 132 120 -84 108 -96 20 -32 -70 -44 18 -30 56 16 28 42 -106 -54 80 94 68 -82 -168 -12 -166 -10 -164 -8 -162 -6 -160 -4 -158 -2 -156 -144 -130 -118 -104 -92 -78 -66 -52 -40 -26 -14"
slither1_5x5_txt_solution = "64 -76 -50 62 38 24 -36 -71 -69 -67 -59 -57 -55 -45 -43 -41 -33 -31 -29 -19 -17 -15 90 116 142 154 -128 -102 -114 140 -88 48 22 -34 74 -60 46 58 72 -100 -86 112 126 -152 -138 98 110 -124 -150 136 148 -134 -122 146 132 120 -84 108 -96 -70 56 -44 20 -32 18 -30 16 28 42 -106 -54 80 94 68 -82 -168 -12 -166 -10 -164 -8 -162 -6 -160 -4 -158 -2 -156 -144 -130 -118 -104 -92 -78 -66 -52 -40 -26 -14"
sol4 = slither1_5x5_txt_solution
solution3=decodeLine enco slither2_txt_solution
solution4=decodeLine enco sol4
filtersolution3 = filter (\x -> let (a,b)=x in a>=0 &&  b>=0 ) solution3
filtersolution4 = filter (\x -> let (a,b)=x in a>=0 &&  b>=0 ) solution4
-- this solution4 is not really solution. It yields two disconnected loops 
-- which is prohibited. Loop in slitherlink puzzle should be connected, i.e. only one.
-- Therefore SAT-solver seems not optimal in this puzzle.
-- It maybe gives us some info for problem, e.g. quick start, maybe we need just a little
-- modification to solve it. 
-- Also, maybe we could to concrete problem, add or disallow certain links.
-- or disallow concrete uncorrect loop and simmilar loops of various sizes.
