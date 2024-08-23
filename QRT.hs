module QRT where

data Cat = E | T | Func Cat Cat
    deriving (Eq)

instance Show Cat where
    show :: Cat -> String
    show E = "e"
    show T = "t"
    show (Func a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"


data TypeS = Leaf Cat | Tr Int | Node TypeS TypeS | Lambda Int TypeS
    deriving (Show)

instance Eq TypeS where
    Leaf x == Leaf y = x == y
    Tr i == Tr j = i == j
    Lambda i x == Lambda j y = x == y && i == j
    Node x1 y1 == Node x2 y2 = (x1 == x2 && y1 == y2) || (x1 == y2 && x2 == y1)
    _ == _ = False

-- some basic types
t, et, e :: TypeS
t = Leaf T
et = Leaf (Func E T)
e = Leaf E
ett = Leaf (Func (Func E T) T)
eet = Leaf  (Func E (Func E T))

tree1 :: TypeS
tree1 = Node e et

tree2 :: TypeS
tree2 = Node ett et

absTree1 :: TypeS
absTree1 = Lambda 1 (Node (Tr 1) et)

trTree1 :: TypeS
trTree1 = Node (Tr 1) et

qrTree2 :: TypeS
qrTree2 = Node ett absTree1

-- testLRule :: Bool
-- testLRule = lRule (Seq e e) (Seq t t) (Seq tree1 t)

-- testRRule :: Bool
-- testRRule = rRule (Seq tree1 t) (Seq absTree1 et)

-- testQRExpansion :: Bool
-- testQRExpansion = qrExpansion (Seq qrTree2 t) (Seq tree2 t)

-- testQRReduction :: Bool
-- testQRReduction = qrReduction (Seq tree2 t) (Seq qrTree2 t)