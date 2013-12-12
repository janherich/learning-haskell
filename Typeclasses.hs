data TraficLight = Red | Yellow | Green

instance Eq TraficLight where
    Red == Red = True
    Green == Green = True
    Yellow ==  Yellow = True
    _ == _ = False

instance Show TraficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

class YesNo a where
    yesno :: a -> Bool
    culprit :: a

instance YesNo Integer where
    yesno 0 = False
    yesno _ = True
    culprit = 0

instance YesNo [a] where
    yesno [] = False
    yesno _ = True
    culprit = []

instance YesNo Bool where
    yesno = id
    culprit = False

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
    culprit = Nothing

instance YesNo TraficLight where
    yesno Red = False
    yesno _ = True
    culprit = Red

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- type-fu !

class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
    tofu x = Frank x

data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
