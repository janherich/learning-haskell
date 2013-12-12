import Data.List

rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles longest = [(a, b, c) | c <- [1..longest], b <- [1..c], a <- [1..b], a ^ 2 + b ^ 2 == c ^ 2]

factorial :: (Integral a) => a -> a
factorial 0 = 0
factorial 1 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n -1)

fibSeq :: Int -> [Int]
fibSeq count = [fib n | n <- [0..count]]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

numTell :: (RealFloat a) => a -> String
numTell n
    | n <= 3.0 = "Smallish number..."
    | n <= 7.0 = "That's the right size !"
    | otherwise = "Waaay to big..."
                  
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<= x) xs)
        biggerSorted = quicksort (filter (> x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- type definition could be also written as 'a -> (a -> (a -> a))'
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- function currying
multTwoWithFour :: (Num a) => a -> (a -> a)
multTwoWithFour = multThree 4

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \ x y -> f y x

largestDivisible :: (Integral a) => a
largestDivisible = let p x = mod x 3829 == 0 in head (filter p [100000, 99999..])

makePower :: (Num a) => Int -> a -> a 
makePower pow = \ x -> foldr (*) 1 (take pow (repeat x))

makePower' :: (Num a) => Int -> a -> a
makePower' pow = (^ pow)


