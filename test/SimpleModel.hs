module SimpleModel (simpleModel) where

import Model

x1 = Var "x1"
x1_out = Var "x1_out"
x1_dot = Var "x1_dot"

e1 = Expr "-0.01*x1"
e2 = Expr "-0.-2*(x1-100)"
e3 = Expr "x1"

emptyAssn = Assignment (Var "") (Expr "")

off1 = Mode "Off1" (Flow [Assignment x1_dot e1, Assignment x1_out e3])
on1 = Mode "On1" (Flow [Assignment x1_dot e2, Assignment x1_out e3])

t1 = Transition on1 off1 (Guard (Expr "x1 <= 20")) (Reset [emptyAssn])
t2 = Transition off1 on1 (Guard (Expr "x1 >= 30")) (Reset [emptyAssn])

simpleModel = Model [off1,on1] [t1,t2]

