

data Tree a = Node a (Tree a) (Tree a) | Leaf a



test1, test2 :: Tree Int
test1 = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)
test2 = Node 1 (Node 2 (Leaf 4) (Leaf 4)) (Leaf 4)

-- findn :: Tree Int -> Int -> Bool
-- findn (Leaf a) n = a == n 
-- findn (Node a x y) n = case a == n of 
--     True -> True
--     False -> findn x n || findn y n


-- return path to value 1 right, 0 left, Nothing none
findn :: Tree Int -> Int -> Maybe [[Int]]
findn (Leaf a) n = if a == n then Just [[]] else Nothing
findn (Node a left right) n = if a == n then Just [[]] else 
    case (findn left n, findn right n) of
        (Nothing, Nothing) -> Nothing
        (Just xs, Nothing) -> Just (map (0 :) xs)
        (Nothing, Just xs) -> Just (map (1 :) xs)
        (Just xs, Just ys) -> Just ((map (0 :) xs) ++ (map (1 :) ys))


-- findn (Node a (Leaf b) (Leaf c)) n = if a == n then Just [] else
--     if b == n then Just [0] else if c == n then Just [1]
-- findn (Node a (Node b x1 y1) (Node c x1 y1)) n = if a == n then Just [] else 
--     if 

--     case b  of
--         (Leaf b) -> Just [27]
--         (Node n _ _) -> Just [83]
--         _ -> case right of 
--             (Leaf n) -> Just [1]
--             (Node n _ _) -> Just [1]
--             _ -> case (findn left n, findn right n) of
--                 (Nothing, Nothing) -> Nothing
--                 (Just xs, _) -> Just (0 : xs)
--                 (_, Just xs) -> Just (1 : xs)
    