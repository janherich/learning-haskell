import Data.List

type Pos = Int
type Height = Int
type Ground =  (Height,Pos)
type Relief = [Ground]

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

route :: Pos -> Pos -> (Relief,Relief,Relief) -> Ground -> (Relief,Relief,Relief)
route pl ph (f,m,l) (h,p)
    | (p > pl) && (p < ph) = (f,(h,p):m,l)
    | p <= pl = ((h,p):f,m,l)
    | p >= ph = (f,m,(h,p):l)
    | otherwise = (f,m,l)

summarize :: Height -> Relief -> Int
summarize t = sum . map (\(h,_) -> t - h)

split :: Relief -> (Relief,Int,Relief)
split ((h1,p1):(h2,p2):xs) = prepare h2 . foldl (route pl ph) ([],[],[]) $ ((h1,p1):(h2,p2):xs)
    where pl = min p1 p2
          ph = max p1 p2
          prepare t (f,m,l) = (f,summarize t m,l)
split _ = ([],0,[])

processRec :: Relief -> Int
processRec = process . split . reverse . sort
    where process ([],m,[]) = m
          process (f,m,l) = processRec f + m + processRec l

calculatePuddle :: [Int] -> Int
calculatePuddle = processRec . snd . mapAccumL indexFold 0
    where indexFold acc n = (succ acc, (n,acc))
