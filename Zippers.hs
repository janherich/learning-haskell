data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                      (Node 'N' Empty Empty)  
                      (Node 'T' Empty Empty)  )  
            (Node 'Y'  
                      (Node 'S' Empty Empty)  
                      (Node 'A' Empty Empty)  )  )  
        (Node 'L'  
            (Node 'W'  
                      (Node 'C' Empty Empty)  
                      (Node 'R' Empty Empty)  )  
            (Node 'A'  
                      (Node 'A' Empty Empty)  
                      (Node 'C' Empty Empty)  )  )

data Direction = L | R deriving (Show, Eq)
type Directions = [Direction]

changeToP:: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node a l r) = Node a (changeToP ds l) r
changeToP (R:ds) (Node a l r) = Node a l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node a l r) = elemAt ds l
elemAt (R:ds) (Node a l r) = elemAt ds r
elemAt [] (Node a _ _) = a

data Track a = LeftTrack a (Tree a) | RightTrack a (Tree a) deriving (Show)
type Tracks a = [Track a]

goLeft :: (Tree a, Tracks a) -> (Tree a, Tracks a)
goLeft (Node x l r, tr) = (l, LeftTrack x r:tr)

goRight :: (Tree a, Tracks a) -> (Tree a, Tracks a)
goRight (Node x l r, tr) = (r, RightTrack x l:tr)

goUp :: (Tree a, Tracks a) -> (Tree a, Tracks a)
goUp (t, LeftTrack x r:tr) = (Node x t r, tr)
goUp (t, RightTrack x l:tr) = (Node x l t, tr)

type Zipper a = (Tree a, Tracks a)

modify :: (a -> a) -> Zipper a -> Zipper a  
modify f (Node x l r, bs) = (Node (f x) l r, bs)  
modify f (Empty, bs) = (Empty, bs) 

x -: f = f x
