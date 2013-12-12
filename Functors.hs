import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.Foldable as F

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right) 
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise = Nothing

x -: f = f x

-- chaining monads explicitly
monadicFn :: Maybe Bool
monadicFn = Just 9 >>= (\x -> Just (x > 8))

-- the same with do syntax
monadicFn' :: Maybe Bool
monadicFn' = do
  x <- Just 9
  Just (x > 8)

lengthCompare :: String -> String -> Ordering
lengthCompare x y = let vowels = length . filter (`elem` "aeiou")
                    in (length x `compare` length y) `mappend`
                       (vowels x `compare` vowels y) `mappend`
                       (x `compare` y)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

-- types kata

class Database s where
    transact :: (a -> a) -> s a -> s a

data Db a = EOF | RecordsDb a (Db a) deriving (Show, Read, Eq)

instance Database Db where
    transact _ EOF = EOF
    transact f (RecordsDb a EOF) = (RecordsDb a (RecordsDb (f a) EOF))
    transact f (RecordsDb a next) = RecordsDb a (transact f next)
