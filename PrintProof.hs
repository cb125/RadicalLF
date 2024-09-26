module PrintProof where

import Prelude hiding ((<>))
import QRT
import QRTProofSearch
import TreeUtils
import Text.PrettyPrint

printProofs :: [ProofTree] -> Doc
printProofs ps = vcat $ map (\p -> (char ' ' $+$ prettyProofTree p)) ps

prettyProofTree :: ProofTree -> Doc
prettyProofTree (LRule seq t1 t2) =
  text "  " <> (prettyProofTree t2 $+$ prettyProofTree t2 $+$ prettySequent seq) <> text " :L"
prettyProofTree (RRule seq t) =
  text "  " <> (prettyProofTree t $+$ prettySequent seq) <> text " :R"
prettyProofTree (QRE seq t) =
  text "  " <> (prettyProofTree t $+$ prettySequent seq) <> text " :QRE"
prettyProofTree (QRR seq t) =
  text "  " <> (prettyProofTree t $+$ prettySequent seq) <> text " :QRR"
prettyProofTree (Axiom seq) = text "  " <> (prettySequent seq) <> text " :Ax"
prettyProofTree (NoRule seq) = text "  " <> (prettySequent seq) <> text " :NR"

prettySequent :: Sequent -> Doc
prettySequent s = text (show s)

