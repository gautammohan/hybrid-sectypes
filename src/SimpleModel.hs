{-|

SimpleModel is a carefully hand-coded structure meant to mirror simple_model.slx to serve as a "unit test" to verify the functions in ParseJSON work.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module SimpleModel (simpleModel, simpleModel2, simplePar) where

import Model

x1 = CVar "x1"
x1_out = CVar "x1_out"
x1_dot = CVar "x1_dot"

e1 = CExpr "-0.01*x1"
e2 = CExpr "-0.01*(x1-100)"
e3 = CExpr "x1"

emptyAssn = CAssignment (CVar "") (CExpr "")

off1 = CMode "Off1" (CFlow [CAssignment x1_dot e1, CAssignment x1_out e3])
on1 = CMode "On1" (CFlow [CAssignment x1_dot e2, CAssignment x1_out e3])
start = CMode "InitialTransition" (CFlow [])

t1 = CTransition start on1 (CGuard (CExpr "")) (CReset [CAssignment x1 (CExpr "10")])
t2 = CTransition off1 on1 (CGuard (CExpr "x1<=20")) (CReset [emptyAssn])
t3 = CTransition on1 off1 (CGuard (CExpr "x1>=30")) (CReset [emptyAssn])

-- | The model itself!
simpleModel = CModel [off1,on1] [t1,t3,t2]

simpleModel2 = CModel [CMode "foo" (CFlow [CAssignment (CVar "x") (CExpr "")])] []
simplePar = CParallel [simpleModel2, simpleModel2]
