import QRT 
import QRTProofSearch
import TreeUtils
import PrintProof

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


tree3 :: TypeS
tree3 = Node et (Node et (Node ett et))

test = findAllProofs (NoRule (Seq tree2 t))

display = PrintProofs test
