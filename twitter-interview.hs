import Data.List

-- type synonyms

type Pos = Int
type Height = Int
type Ground =  (Height,Pos)
type Relief = [Ground]

-- test cases

case1 :: [Int]
case1 = [2,5,1,2,3,4,7,7,6]

case2 :: [Int]
case2 = [2,1,3,1,2,5]

case3 :: [Int]
case3 = [7,5,6,2,4,5,2,1,3,4,1]

case4 :: [Int]
case4 = [4,3,2,1,2,3,4]

case5 :: [Int]
case5 = [1,4,3,4,3,4,1]

case6 :: [Int]
case6 = [6,1,1,5]

-- first recursive solution (very ineffective and complicated)

route :: Pos -> Pos -> (Relief,Relief,Relief) -> Ground -> (Relief,Relief,Relief)
route pl ph tr@(f,m,l) g@(h,p)
    | (p > pl) && (p < ph) = (f,g:m,l)
    | p <= pl = (g:f,m,l)
    | p >= ph = (f,m,g:l)
    | otherwise = tr

summarize :: Height -> Relief -> Int
summarize t = sum . map (\(h,_) -> t - h)

split :: Relief -> (Relief,Int,Relief)
split rl@((h1,p1):(h2,p2):xs) = prepare h2 . foldl (route pl ph) ([],[],[]) $ rl
    where pl = min p1 p2
          ph = max p1 p2
          prepare t (f,m,l) = (f,summarize t m,l)
split _ = ([],0,[])

processRec :: Relief -> Int
processRec = process . split . reverse . sort
    where process ([],m,[]) = m
          process (f,m,l) = processRec f + m + processRec l

calculatePuddle1 :: [Int] -> Int
calculatePuddle1 = processRec . snd . mapAccumL indexFold 0
    where indexFold acc n = (succ acc, (n,acc))

-- second solution (much more effective and simpler)

mapBounds :: [Int] -> [Int]
mapBounds relief@(x:xs) = snd $ mapAccumL process x relief
    where process m n = let cm = max m n in (cm, cm - n)
                                         
calculatePuddle2 :: [Int] -> Int
calculatePuddle2 relief = sum $ zipWith min (mapBounds (reverse relief)) (mapBounds relief) 
