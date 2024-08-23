-- start with a binary tree
import Data.Maybe (fromJust)

data Tree a = Leaf a | Node a (Tree a) (Tree a) 
    deriving (Eq, Show)

data Cxt a = Top | L (Tree a) (Cxt a) a | R (Tree a) (Cxt a) a
    deriving (Eq, Show)

data TreeZipper a = TreeZip (Tree a) (Cxt a) 
    deriving (Eq, Show)


psTree :: Tree String
psTree = Node "TP" (Leaf "DP") (Node "T'" (Leaf "T") (Node "VP" (Leaf "V") (Leaf "DP")))

toTreeZipper :: Tree a -> TreeZipper a
toTreeZipper t = TreeZip t Top

psTreeZip :: TreeZipper String
psTreeZip = toTreeZipper psTree

-- move up, right, left

parent :: TreeZipper a -> Maybe (TreeZipper a)
parent (TreeZip _ Top) = Nothing
parent (TreeZip t c) = case c of
    (L r parentC label) -> Just (TreeZip (Node label t r) parentC)
    (R l parentC label) -> Just (TreeZip (Node label t l) parentC)

right :: TreeZipper a -> Maybe (TreeZipper a)
right (TreeZip (Leaf _) _) = Nothing
right (TreeZip (Node label l r) c) = Just (TreeZip r (R l c label)) 

left :: TreeZipper a -> Maybe (TreeZipper a)
left (TreeZip (Leaf _) _) = Nothing 
left (TreeZip (Node label l r) c) = Just (TreeZip l (L r c label))

-- search and traverse the tree using zippers
find :: TreeZipper a -> (TreeZipper a -> Bool) -> [TreeZipper a]
find tloc test = if test tloc then 
    case tloc of 
        (TreeZip (Leaf _) _) -> [tloc]
        _ -> tloc : find (fromJust (left tloc)) test ++ find (fromJust (right tloc)) test
    else case tloc of 
        (TreeZip (Leaf _) _) -> []
        _ -> find (fromJust (left tloc)) test ++ find (fromJust (right tloc)) test

replace :: TreeZipper a -> a -> TreeZipper a
replace (TreeZip (Leaf label) c) newLabel = TreeZip (Leaf newLabel) c
replace (TreeZip (Node label l r) c) newLabel = TreeZip (Node newLabel l r) c

getVal :: TreeZipper a -> a
getVal (TreeZip (Leaf label) _) = label
getVal (TreeZip (Node label _ _) _) = label


-- Thesesus and the Zipper

-- data Node a = DeadEnd a
--             | Passage a (Node a)
--             | Fork a (Node a) (Node a)

-- get:: Node a -> a
-- get (DeadEnd x) = x 
-- get (Passage x _) = x 
-- get (Fork x _ _) = x

-- put :: a -> Node a -> Node a 
-- put x (DeadEnd _) = DeadEnd x
-- put x (Passage _ n) = Passage x n 
-- put x (Fork _ n1 n2 ) = Fork x n1 n2 

-- labyrinth :: Node (Int, Int) 
-- labyrinth = Fork (0, 2) 
--     (Passage (2, 0) 
--         (Fork (2, 0) 
--             (DeadEnd (0, -1)) 
--             (Passage (0, 1) 
--                 (DeadEnd (0, 0))))) 
--     (Fork (-2, 0) 
--         (DeadEnd (-1, 0)) 
--         (DeadEnd (0, -2)))

-- turnRight :: Node a -> Maybe (Node a)
-- turnRight (Fork _ l r) = Just r
-- turnRight _            = Nothing

-- data Branch = KeepStraight | TurnLeft | TurnRight
-- type Thread = [Branch]

-- retrieve :: Thread -> Node a -> a 
-- retrieve []                n             = get n
-- retrieve (KeepStraight:ts) (Passage _ n) = retrieve ts n
-- retrieve (TurnRight   :ts) (Fork _ _ r)  = retrieve ts r
-- retrieve (TurnLeft    :ts) (Fork _ l _)  = retrieve ts l

-- update :: (a -> a) -> Thread -> Node a -> Node a 
-- update f []                n             = put (f (get n)) n
-- update f (KeepStraight:ts) (Passage d n) = Passage d (update f ts n)
-- update f (TurnRight   :ts) (Fork d l r)  = Fork d l (update f ts r)
-- update f (TurnLeft    :ts) (Fork d l r)  = Fork d (update f ts l) r
