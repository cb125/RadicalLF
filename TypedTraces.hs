
data Cat = E | T | Func Cat Cat 
    -- deriving (Show)

type Word = String

instance Show Cat where
    show E = "e"
    show T = "t"
    show (Func a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

data LF = Leaf Cat | Node LF LF | Tr Int Cat | AbsNode Int LF

type Derivation = [LF]


findTraceType :: LF -> Int -> [Cat]
findTraceType (Tr i x) n = case i of
    n -> [x]
findTraceType (Leaf _) _ = []
findTraceType (Node x y) n = findTraceType x n ++ findTraceType y n
findTraceType (AbsNode _ x) n = findTraceType x n

getType :: LF -> Maybe Cat 
getType (Leaf x) = Just x
getType (Tr _ x) = Just x
getType (AbsNode i x) = case findTraceType x i of 
    [y] -> case getType x of
        Nothing -> Nothing
        Just z -> Just (Func y z)
    _ -> Nothing
getType (Node x y) = fa (getType x) (getType y) 
    
-- rename this later
fa :: Maybe Cat -> Maybe Cat -> Maybe Cat 
fa (Just a) (Just b) = case (a,b) of
    (_, Func a x) -> Just x
    (Func b x, _) -> Just x
    _ -> Nothing
fa _ _ = Nothing

eat :: LF 
eat = Leaf (Func E (Func E T))

john :: LF
john = Leaf E

cake :: LF
cake = Leaf E

everyone :: LF 
everyone = Leaf (Func (Func E T) T)

s1 :: LF
s1 = Node john (Node eat cake)

s2 :: LF
s2 = Node eat cake

s3 :: LF 
s3 = Node john (Node eat everyone)

s4 :: LF 
s4 = Node everyone (AbsNode 1 (Node john (Node eat (Tr 1 E))))

s5 :: LF 
s5 = Node everyone (AbsNode 1 (Node (Tr 1 E) (Node eat john)))


-- QRT Derivation Rules

