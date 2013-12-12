import Data.Monoid
import Data.Ratio
import Data.List (all)

-- writer monad

newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a*b)

tell :: Monoid m => m -> Writer m ()
tell m = Writer ((), m)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a 0 = do
  tell ["Finished with " ++ show a]
  return a
gcd' a b = do
  tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
  gcd' b (a `mod` b)

-- state monad

type Stack = [Int]

pop :: Stack -> (Int,Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((),a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let ((), newStack1) = push 3 stack
                       (a , newStack2) = pop newStack1
                   in pop newStack2

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState

pop' :: State Stack Int
pop' = State $ \(x:xs) -> (x,xs)

push' :: Int -> State Stack ()
push' a = State $ \xs -> ((),a:xs)

stackManip' :: State Stack Int
stackManip' = do
  push' 3
  pop'
  pop'

stackStuff :: State Stack ()
stackStuff = do
  a <- pop'
  if a == 5
     then push' 5
     else do
       push' 3
       push' 8

-- custom monad

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving (Show)

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

theSituation :: Prob (Prob Char)
theSituation = Prob
               [ ( Prob [('a',1%2),('b',1%2)] , 1%4 )
               , ( Prob [('c',1%2),('d',1%2)] , 3%4 ) ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = let multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs
                    in Prob $ concat $ map multAll xs

instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (==Tails) [a,c])
