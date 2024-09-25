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


