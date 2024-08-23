module QRTProofSearch where

import QRTProofs
import QRT
import Data.Maybe
import Data.Tuple


-- given a sequent find all LRule steps that could apply. 

-- find functional leaf 

applyLRule :: Sequent -> [Maybe (ProofTree, ProofTree)]
applyLRule (Seq ts1 ts2) = case findFunc ts1 of
    [] -> []
    pathList -> case getParentPaths pathList of 
        Nothing -> []
        Just sigmaPaths -> zipWith3 fillProof (map catToTS bs) deltas replacedTrees
            where deltas = map (getSister ts1) pathList -- [Maybe TypeS]
                  fBtoA = map (getCat . getFromPath ts1) pathList -- [Maybe Cat]
                  as = map getResultType fBtoA -- [Maybe Cat]
                  bs = map getArgType fBtoA -- [Maybe Cat]
                  Just replacedTrees = replaceAll ts1 sigmaPaths (map catToTS as)
                  fillProof b d r = if isNothing b || isNothing d || isNothing r then Nothing
                      else Just (NoRule (Seq (fromJust d) (fromJust b)), NoRule (Seq (fromJust r) ts2))


applyAxiomRule :: Sequent -> [ProofTree]
applyAxiomRule (Seq ts1 ts2) = [Axiom (Seq ts1 ts2) | ts1 == ts2]

-- Not sure I really want to use this type for this argument (why not just give the top sequent if there is one, rather than returning the proofTree and then I use findAllProofs to handle the rest)
applyRRule :: Sequent -> [ProofTree]
applyRRule (Seq (Lambda n ts) t) = case t of 
    Leaf (Func t1 t2) -> 
        [RRule (Seq (Lambda n ts) t) (NoRule (Seq (replaceTrace n ts (Leaf t1)) (Leaf t2)))]
    _ -> [] 
applyRRule _ = []

applyQRE :: Sequent -> [ProofTree]
applyQRE (Seq ts1 ts2) = map f (iterateQR ts1 1)
    where f (first, second) = QRE (Seq ts1 ts2) (NoRule (Seq (Node first (Lambda 1 second)) ts2))
        

iterateQR :: TypeS -> Int -> [(TypeS, TypeS)]
iterateQR (Leaf x) n = [(Leaf x, Tr n)]
iterateQR (Tr n1) n = [] -- do we want to allow QR of traces
iterateQR (Node t1 t2) n = 
    (Node t1 t2, Tr n) : map (applysecond (`Node` t2)) (iterateQR t1 n) ++ map (applysecond (t1 `Node`)) (iterateQR t2 n)
        where applysecond f (first, second) = (first, f second)
iterateQR (Lambda n1 t) n = map (applysecond (Lambda n1)) (iterateQR t n)
        where applysecond f (first, second) = (first, f second)
              

betaReduce :: TypeS -> TypeS -> TypeS
betaReduce (Lambda n ts1)= replaceTrace n ts1

findAllProofs :: ProofTree -> [ProofTree]
findAllProofs (Axiom x) = [Axiom x]
findAllProofs (LRule x p1 p2) = zipWith (LRule x) (findAllProofs p1) (findAllProofs p2)
findAllProofs (NoRule s) = iterateL lProofs ++ aProofs ++ rProofs ++ searchQR qrProofs
    where lProofs = applyLRule s
          iterateL p = case p of 
            [] -> []
            (x:xs) -> case x of 
                    Nothing -> []
                    Just (p1, p2) -> zipWith (LRule s) (findAllProofs p1) (findAllProofs p2) ++ iterateL xs
          aProofs = applyAxiomRule s
          rProofs = case applyRRule s of 
            [] -> []
            [RRule s p] -> map (RRule s) (findAllProofs p)
          qrProofs = applyQRE s
          searchQR p = case p of
            [] -> []
            (QRE s1 (NoRule (Seq t1 t2))):xs -> if nFuncTS t1 >= nLambdas t1 
                then map (QRE s1) (findAllProofs (NoRule (Seq t1 t2))) ++ searchQR xs else []
            -- _ -> []


testSearch = NoRule (Seq tree2 t) 



-- reconcile this with other replacement functions in QRT script
replace :: TypeS -> MyPath -> Maybe TypeS -> Maybe TypeS
replace _ _ Nothing = Nothing
replace _ [] ts2 = ts2
replace ts1 (x:xs) ts2 = case ts1 of
    Node left right -> if x == MyLeft then
        case replace left xs ts2 of
            Nothing -> Nothing
            Just ts3 -> Just (Node ts3 right)
        else case replace right xs ts2 of
            Nothing -> Nothing
            Just ts3 -> Just (Node left ts3)
    Lambda n right -> if x == MyLeft then Nothing else
        case replace right xs ts2 of
            Nothing -> Nothing
            Just ts3 -> Just (Lambda n ts3)
    _ -> Nothing


replaceAll :: TypeS -> [MyPath] -> [Maybe TypeS] -> Maybe [Maybe TypeS]
replaceAll ts [] [] = Just []
replaceAll ts (x:xs) (y:ys) = case replaceAll ts xs ys of
    Nothing -> Nothing
    Just tslist -> Just (replace ts x y : tslist)
replaceAll _ _ _ = Nothing




getResultType :: Maybe Cat -> Maybe Cat
getResultType x = case x of
    Nothing -> Nothing
    Just (Func a b) -> Just b
    Just _ -> Nothing

getArgType :: Maybe Cat -> Maybe Cat
getArgType x = case x of
    Nothing -> Nothing
    Just (Func a b) -> Just a
    Just _ -> Nothing

-- Location of all functional leaves. 
findFunc :: TypeS -> [MyPath]
findFunc (Leaf (Func t1 t2)) = [[]]
findFunc (Leaf _) = []
findFunc (Tr _) = []
findFunc (Node a b) = case (findFunc a, findFunc b) of
    ([], []) -> []
    (xs, []) -> map (MyLeft :) xs
    ([], ys) -> map (MyRight :) ys
    (xs, ys) -> map (MyLeft :) xs ++ map (MyRight :) ys
findFunc (Lambda n ts) = case findFunc ts of
    [] -> []
    xs -> map (MyRight :) xs

nFuncTS :: TypeS -> Int
nFuncTS (Tr _) = 0
nFuncTS (Leaf t) = nFuncCat t
nFuncTS (Node t1 t2) = nFuncTS t1 + nFuncTS t2
nFuncTS (Lambda n t) = nFuncTS t

nFuncCat :: Cat -> Int
nFuncCat (Func t1 t2) = 1 + nFuncCat t1 + nFuncCat t2 
nFuncCat _ = 0

nLambdas :: TypeS -> Int
nLambdas (Node t1 t2) = nLambdas t1 + nLambdas t2
nLambdas (Lambda n t) = 1 + nLambdas t
nLambdas n = 0

tree3 :: TypeS
tree3 = Node et (Node et (Node ett et))