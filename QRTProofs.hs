module QRTProofs where 

import QRT


data ProofTree = LRule Sequent ProofTree ProofTree | RRule Sequent ProofTree | QRE Sequent ProofTree | QRR Sequent ProofTree | Axiom Sequent | NoRule Sequent  
    deriving Show

proofCheck :: ProofTree -> Maybe Sequent
proofCheck (Axiom (Seq a b)) = if a == b then Just (Seq a b) else Nothing
proofCheck (LRule (Seq a b) pr1 pr2) = case proofCheck pr1 of
    Nothing -> Nothing
    Just x -> case proofCheck pr2 of 
        Nothing -> Nothing
        Just y -> if lRule x y (Seq a b) then Just (Seq a b) else Nothing
proofCheck (RRule (Seq a b) pr1) = case proofCheck pr1 of 
    Nothing -> Nothing
    Just x -> if rRule x (Seq a b) then Just (Seq a b) else Nothing
proofCheck (QRE (Seq a b) pr1) = case proofCheck pr1 of 
    Nothing -> Nothing
    Just x -> if qrExpansion x (Seq a b) then Just (Seq a b) else Nothing
proofCheck (QRR (Seq a b) pr1) = case proofCheck pr1 of
    Nothing -> Nothing
    Just x -> if qrReduction x (Seq a b) then Just (Seq a b) else Nothing
proofCheck (NoRule _) = Nothing

proof0 = LRule (Seq tree1 t) (Axiom (Seq e e)) (Axiom (Seq t t))
proof1 = QRE (Seq tree2 t) (LRule (Seq qrTree2 t) (RRule (Seq absTree1 et) (LRule (Seq (Node e et) t) (Axiom (Seq e e)) (Axiom (Seq t t)))) (Axiom (Seq t t)))

subproof1 = proof1
subproof2 = LRule (Seq qrTree2 t) (RRule (Seq absTree1 et) (LRule (Seq (Node e et) t) (Axiom (Seq e e)) (Axiom (Seq t t)))) (Axiom (Seq t t))
subproof3 = Axiom (Seq t t)
subproof4 = RRule (Seq absTree1 et) (LRule (Seq (Node e et) t) (Axiom (Seq e e)) (Axiom (Seq t t)))
subproof5 = LRule (Seq (Node e et) t) (Axiom (Seq e e)) (Axiom (Seq t t))
subproof6 = Axiom (Seq e e)
