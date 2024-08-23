module QRTProofSearch where

-- import QRTProofs
import QRT
import TreeUtils
import Data.Maybe
import Data.Tuple
import Control.Monad

data Sequent = Seq TypeS TypeS
    deriving (Show, Eq)

data ProofTree = LRule Sequent ProofTree ProofTree
               | RRule Sequent ProofTree
               | QRE Sequent ProofTree
               | QRR Sequent ProofTree
               | Axiom Sequent
               | NoRule Sequent deriving (Eq, Show)

-- given a sequent find all LRule steps that could apply. 

-- find functional leaf 

isFunc :: TSLoc -> Bool
isFunc (Loc (Leaf (Func _ _)) _) = True
isFunc _ = False

getArgType :: Maybe TypeS -> Maybe TypeS
getArgType (Just (Leaf (Func a b))) = Just (Leaf a)
getArgType _ = Nothing 

getResultType :: Maybe TypeS -> Maybe TypeS
getResultType (Just (Leaf (Func a b))) = Just (Leaf b)
getResultType _ = Nothing

applyLRule :: Sequent -> [Maybe (ProofTree, ProofTree)]
applyLRule (Seq ts1 ts2) = zipWith3 fillPrf bs deltas rs
    where ts1zip        = toZip ts1
          fBtoAlocs     = find ts1zip isFunc
          fBtoAs        = map (Just . getSubTreeAt) fBtoAlocs
          as            = map getResultType fBtoAs
          bs            = map getArgType fBtoAs
          deltaLocs     = map sister fBtoAlocs
          deltas        = map (>>= (Just . getSubTreeAt)) deltaLocs
          sigmaLocs     = map parent fBtoAlocs
          rLocs         = zipWith (liftM2 replaceSubTree) sigmaLocs as 
          rs            = map (>>= (Just . getSubTreeAt)) rLocs
          fillPrf b d r = if isNothing b || isNothing d || isNothing r then Nothing 
            else Just (NoRule (Seq (fromJust d) (fromJust b)), NoRule (Seq (fromJust r) ts2))
            

applyAxiomRule :: Sequent -> [ProofTree]
applyAxiomRule (Seq ts1 ts2) = [Axiom (Seq ts1 ts2) | ts1 == ts2]

applyRRule :: Sequent -> [ProofTree]
applyRRule (Seq (Lambda n ts) t) = case t of 
    Leaf (Func t1 t2) -> 
        [RRule (Seq (Lambda n ts) t) (NoRule (Seq d (Leaf t2)))]
        where isTrace loc = case loc of
                (Loc (Tr n) _) -> True
                _              -> False
              trlocs = find (toZip ts) isTrace
              d = case map (`replaceSubTree` Leaf t1) trlocs of
                [x] -> toTree x
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


-- testSearch = NoRule (Seq tree2 t) 


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