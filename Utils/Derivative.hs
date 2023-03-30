module Utils.Derivative (derivative, allDerivatives) where

import Utils.DataTypes
import Utils.Simplify
import Utils.ComprExpr

-- pochodna po uproszczeniu
derivative :: ExprThree -> Var -> ExprThree
derivative expr v = simplify (derivativeUnS expr v)

-- pochodne po zmiennych (x, y, z)
allDerivatives :: ExprThree -> (ExprThree, ExprThree, ExprThree)
allDerivatives expr = (derivative expr X, derivative expr Y, derivative expr Z)

-- nieuproszczona pochodna - derivative unsimplified
derivativeUnS :: ExprThree -> Var -> ExprThree

derivativeUnS (Const _) _ = Const (Num 0)
derivativeUnS (Var w) v 
    | w == v    = Const (Num 1)
    | True      = Const (Num 0)

-- pochodne operacji od dwóch funkcji
-- (f+g)' = f' + g'
-- (f*g)' = f'*g + g'*f
-- (f/g)' = (f'*g - g'*f)/(g*g)
-- (f-g)' = f' - g'
derivativeUnS (ListOp Add list) v = ListOp Add (map ((flip derivativeUnS) v) list)

derivativeUnS (ListOp Mul []) v = Const (Num 0)
derivativeUnS (ListOp Mul (e:es)) v = ListOp Add 
    [ListOp Mul ((derivativeUnS e v) : es), ListOp Mul [derivativeUnS (ListOp Mul es) v, e] ]

derivativeUnS (TwoArgOp l Div r) v = TwoArgOp 
    (ListOp Add [ListOp Mul [derivativeUnS r v, l], ListOp Mul [Const (Num (-1)), r, derivativeUnS l v]]) 
    Div 
    (Apply (Pow 2) r)

derivativeUnS (TwoArgOp l Substr r) v = TwoArgOp (derivativeUnS l v) Substr (derivativeUnS r v)

-- obliczenie pochodnej funkcji, gdy jej argumentem jest zmienna po której różniczkujemy
-- w przeciwnym wypadku traktujemy ją jak stałą
-- funkcję od stałej traktujemy jako stałą - pochodna jest równa 0
derivativeUnS (Apply f (Var w)) v 
    | w == v    = funcDerivative f (Var v)
    | True      = Const (Num 0)
derivativeUnS (Apply f (Const c)) v = Const (Num 0)

-- pochodna funkcji od wyrażenia jako pochodna złożenia
-- (f(g(x)))' = f'(g(x))*g'(x)
derivativeUnS (Apply f expr) v = ListOp Mul [funcDerivative f expr, derivativeUnS expr v]

-- pochodne funkcji
-- sin' = cos
-- cos' = -sin
-- tg' = 1/(cos^2)
-- exp' = exp
-- ln' = 1/x
funcDerivative :: Func -> ExprThree -> ExprThree
funcDerivative Sin expr = Apply Cos expr
funcDerivative Cos expr = ListOp Mul [Const (Num (-1)), Apply Sin expr]
funcDerivative Tg expr = TwoArgOp (Const (Num 1)) Div (Apply (Pow 2) (Apply Cos expr))
funcDerivative Exp expr = Apply Exp expr
funcDerivative Ln expr = TwoArgOp (Const (Num 1)) Div expr
funcDerivative (Pow n) expr = ListOp Mul [Const (Num n), Apply (Pow (n-1)) expr]