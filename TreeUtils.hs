module TreeUtils where

import QRT
import Data.Maybe

-- Zippers for Type Structures
data TSCxt = Top | L TypeS TSCxt | R TypeS TSCxt | Abs Int TSCxt
    deriving (Eq, Show)

data TSLoc = Loc TypeS TSCxt
    deriving (Eq, Show)

toZip :: TypeS -> TSLoc
toZip ts = Loc ts Top

toTree :: TSLoc -> TypeS
toTree (Loc ts Top) = ts
toTree loc = (toTree . fromJust . parent) loc

parent :: TSLoc -> Maybe TSLoc
parent (Loc _ Top) = Nothing
parent (Loc ts cxt) = case cxt of
    L right parent -> Just (Loc (Node ts right) parent)
    R left parent -> Just (Loc (Node left ts) parent)
    Abs ix parent -> Just (Loc (Lambda ix ts) parent)

sister :: TSLoc -> Maybe TSLoc
sister (Loc ts cxt) = case cxt of
    L right parent -> Just (Loc right (R ts cxt))
    R left parent -> Just (Loc left (L ts cxt))
    _ -> Nothing 

downRight :: TSLoc -> Maybe TSLoc
downRight (Loc (Leaf _) _) = Nothing
downRight (Loc (Tr _) _) = Nothing
downRight (Loc (Lambda _ _) _) = Nothing
downRight (Loc (Node l r) cxt) = Just (Loc r (R l cxt))

downLeft :: TSLoc -> Maybe TSLoc
downLeft (Loc (Leaf _) _) = Nothing
downLeft (Loc (Tr _) _) = Nothing
downLeft (Loc (Lambda _ _) _) = Nothing
downLeft (Loc (Node l r) cxt) = Just (Loc l (L r cxt))

downLambda :: TSLoc -> Maybe TSLoc
downLambda (Loc (Leaf _) _) = Nothing
downLambda (Loc (Tr _) _) = Nothing
downLambda (Loc (Node {}) _) = Nothing
downLambda (Loc (Lambda ix r) cxt) = Just (Loc r (Abs ix cxt))
-- downLambda _ = Nothing 

-- find list of all places where condition is true
find :: TSLoc -> (TSLoc -> Bool) -> [TSLoc]
find loc test = case loc of
    Loc (Leaf _) _ -> [loc | test loc]
    Loc (Tr _) _ -> [loc | test loc]
    Loc (Lambda _ r) _ -> [loc | test loc] ++ find (fromJust (downLambda loc)) test
    Loc (Node {}) _ -> [loc | test loc] ++ find (fromJust (downRight loc)) test ++ find (fromJust (downLeft loc)) test

replaceSubTree :: TSLoc -> TypeS -> TSLoc
replaceSubTree (Loc ts1 cxt) ts2 = Loc ts2 cxt

getSubTreeAt :: TSLoc -> TypeS
getSubTreeAt (Loc ts1 cxt) = ts1

