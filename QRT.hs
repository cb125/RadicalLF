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


data Sequent = Seq TypeS TypeS
    deriving (Show, Eq)

-- Thinking about using a prooftree with the rules as labels as a type?

-- data BinaryTree a = Lf a | Nd (BinaryTree a) (BinaryTree a)

data MyDir = MyLeft | MyRight
    deriving (Eq, Ord, Show)

type MyPath = [MyDir]



-- check if `b' is in `a'
contains :: TypeS -> TypeS -> Bool
contains a b = (a == b) || (case a of
    Node x y -> contains x b || contains y b
    Lambda _ x -> contains x b
    _ -> False)

-- find locations of b in a
findTS :: TypeS -> TypeS -> Maybe [MyPath]
findTS a b = if a == b then Just [[]]
    else case a of
        Leaf _ -> Nothing
        Tr _ -> Nothing
        Node left right -> case (findTS left b, findTS right b) of
            (Nothing, Nothing) -> Nothing
            (Just xs, Nothing) -> Just (map (MyLeft :) xs)
            (Nothing, Just xs) -> Just (map (MyRight :) xs)
            (Just xs, Just ys) -> Just (map (MyLeft :) xs ++ map (MyRight :) ys)
        Lambda _ right -> case findTS right b of
            Nothing -> Nothing
            Just xs -> Just (map (MyRight :) xs)


replaceTS :: TypeS -> MyPath -> TypeS -> Maybe TypeS
replaceTS ts1 [] ts2 = Just ts2
replaceTS ts1 (x:xs) ts2 = case ts1 of
    Node left right -> if x == MyLeft then
        case replaceTS left xs ts2 of
            Nothing -> Nothing
            Just ts -> Just (Node ts right)
        else case replaceTS right xs ts2 of
            Nothing -> Nothing
            Just ts -> Just (Node left ts)
    Lambda n right -> if x == MyRight then
        case replaceTS right xs ts2 of
            Nothing -> Nothing
            Just ts -> Just (Lambda n ts)
        else Nothing
    _ -> Nothing


replaceTS2 :: TypeS -> [MyPath] -> [TypeS] -> Maybe [TypeS]
replaceTS2 ts1 [] [] = Just []
replaceTS2 ts1 (x:xs) [] = Nothing
replaceTS2 ts1 [] (x:xs) = Nothing
replaceTS2 ts1 (x:xs) (y:ys) = case replaceTS ts1 x y of
    Nothing -> Nothing
    Just ts -> case replaceTS2 ts1 xs ys of
        Nothing -> Nothing
        Just l1 -> Just (ts : l1)

-- find all instances of ts2 in ts1 and all ways of replacing one with ts3
findReplace :: TypeS -> TypeS -> TypeS -> Maybe [TypeS]
findReplace ts1 ts2 ts3 = case findTS ts1 ts2 of
    Nothing -> Nothing
    Just paths -> replaceTS2 ts1 paths replacements
      where replacements = dupseq [ts3] (length paths)


findLambdas :: TypeS -> [TypeS]
findLambdas (Lambda n ts) = (Lambda n ts) : (findLambdas ts)
findLambdas (Node ts1 ts2) = (findLambdas ts1) ++ (findLambdas ts2)
findLambdas _ = []

getFromPath :: TypeS -> MyPath -> Maybe TypeS
getFromPath ts [] = Just ts
getFromPath (Node left right) (x:xs) = if x == MyLeft then getFromPath left xs 
    else getFromPath right xs
getFromPath (Lambda n right) (x:xs) = if x == MyLeft then Nothing 
    else getFromPath right xs
getFromPath _ _ = Nothing 

getSister :: TypeS -> MyPath -> Maybe TypeS
getSister ts [] = Nothing
getSister (Node left right) [x] = if x == MyLeft then Just right else Just left
getSister (Node left right) (x:xs) = if x == MyLeft then getSister left xs else getSister right xs
getSister _ _ = Nothing

getParentPath :: MyPath -> Maybe MyPath
getParentPath [] = Nothing
getParentPath [x] = Just []
getParentPath (x:xs) = case getParentPath xs of
    Nothing -> Nothing
    Just newpath -> Just (x : newpath)

getParentPaths :: [MyPath] -> Maybe [MyPath]
getParentPaths [] = Just []
getParentPaths [x] = case getParentPath x of 
    Nothing -> Nothing 
    Just newpath -> Just [newpath]
getParentPaths (x:xs) = case getParentPath x of
    Nothing -> Nothing
    Just newpath -> case getParentPaths xs of
        Nothing -> Nothing
        Just newpaths -> Just (newpath : newpaths)


getCat :: Maybe TypeS -> Maybe Cat
getCat Nothing = Nothing
getCat (Just (Leaf c)) = Just c
getCat _ = Nothing


catToTS :: Maybe Cat -> Maybe TypeS
catToTS Nothing = Nothing
catToTS (Just x) = Just (Leaf x)

-- given a function and an argument find result type 
runFA :: Maybe Cat -> Maybe Cat -> Maybe Cat
runFA (Just (Func a b)) (Just (Func c d))
  | a == Func c d = Just b
  | c == Func a b = Just d
  | otherwise = Nothing
runFA (Just (Func a b)) (Just c) = if c == a then Just b else Nothing
runFA (Just a) (Just (Func b c)) = if a == b then Just c else Nothing
runFA _ _ = Nothing


dupseq :: [a] -> Int -> [a]
dupseq xs 0 = []
dupseq xs n = xs ++ dupseq xs (n-1)

-- given a function and a result, find the argument type to get that result
findArg :: Maybe Cat -> Maybe Cat -> Maybe Cat
findArg (Just (Func a b)) (Just c) = if c == b then Just a else Nothing
findArg (Just a) (Just (Func b c)) = if a == c then Just b else Nothing
findArg _ _ = Nothing

getVals :: [Maybe a] -> [a]
getVals [] = []
getVals (x:xs) = case x of
    Nothing -> getVals xs
    Just y -> y : getVals xs


goodPaths :: (Eq b) => [a] -> [Maybe b] -> Maybe [a]
goodPaths [] [] = Just []
goodPaths (x:xs) [] = Nothing
goodPaths [] (x:xs) = Nothing
goodPaths (x:xs) (y:ys) = maybeConcat zs (goodPaths xs ys)
    where zs = if y == Nothing then Just [] else Just [x]


maybeConcat :: Maybe [a] -> Maybe [a] -> Maybe [a]
maybeConcat (Just l1) (Just l2) = Just (l1 ++ l2)
maybeConcat _ _ = Nothing


atLeastOne :: (a -> Bool) -> [a] -> Bool
atLeastOne f [] = False
atLeastOne f [x] = f x
atLeastOne f (x:xs) = f x || atLeastOne f xs

-- see if the first argument is a QRd version of the second argument
qrEquivalent :: TypeS -> TypeS -> Bool
qrEquivalent (Leaf _) _ = False
qrEquivalent (Tr _) _ = False
qrEquivalent (Node a (Lambda n ts1)) ts = (ts2 == ts) || qrEquivalent ts1 ts
    where ts2 = replaceTrace n ts1 a
qrEquivalent (Node a b) ts = qrEquivalent a ts || qrEquivalent b ts
qrEquivalent (Lambda n ts1) ts = qrEquivalent ts1 ts

-- make a smarter version of this that will respect the right binding rules and doesn't ret
replaceTrace :: Int -> TypeS -> TypeS -> TypeS
replaceTrace _ (Leaf t) _ = Leaf t
replaceTrace n (Tr m) ts = if m == n then ts else Tr m
replaceTrace n a b = case a of
    Lambda m c -> Lambda m (replaceTrace n c b)
    Node c d -> Node (replaceTrace n c b) (replaceTrace n d b)



--- QRT Style inference rule checkers

axiomRule :: Sequent -> Bool
axiomRule (Seq a b) = a == b

-- left right bottom
lRule :: Sequent -> Sequent -> Sequent -> Bool
lRule (Seq a b) (Seq c d) (Seq e f) = d == f &&
    (case findTS e a of
        Nothing -> False
        Just allPaths -> case goodPaths allPaths replaceWith of
            Nothing -> False
            Just validPaths -> case replaceTS2 e replacementPaths (map Leaf (getVals replaceWith)) of
                Nothing -> False
                Just l -> atLeastOne (== c) l
              where Just replacementPaths = getParentPaths validPaths
          where replaceWith = map (runFA (getCat (Just b)) . getCat . getSister e) allPaths)

-- top -> bottom
rRule :: Sequent -> Sequent -> Bool
rRule (Seq a b) (Seq (Lambda n c) d) = case findArg (getCat (Just d))  (getCat (Just b)) of
    Nothing -> False
    Just argType -> case findReplace a (Leaf argType) (Tr n) of
        Nothing -> False
        Just candidates -> atLeastOne (== c) candidates
rRule _ _ = False

-- -- QRT Structural Rule Checkers (top -> bottom)
qrExpansion :: Sequent -> Sequent -> Bool
qrExpansion (Seq a b) (Seq c d) = b == d && qrEquivalent a c

qrReduction :: Sequent -> Sequent -> Bool
qrReduction (Seq a b) (Seq c d) = b == d && qrEquivalent c a


--- Testing and definitions 

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

testLRule :: Bool
testLRule = lRule (Seq e e) (Seq t t) (Seq tree1 t)

testRRule :: Bool
testRRule = rRule (Seq tree1 t) (Seq absTree1 et)

testQRExpansion :: Bool
testQRExpansion = qrExpansion (Seq qrTree2 t) (Seq tree2 t)

testQRReduction :: Bool
testQRReduction = qrReduction (Seq tree2 t) (Seq qrTree2 t)