module Utils.DataTypes where

-- KLASA REPREZENTUJĄCA DRZEWO WYRAŻENIA
data ExprThree = Const Const | Var Var | Apply Func ExprThree | ListOp ListOp [ExprThree] | TwoArgOp ExprThree TwoArgOp ExprThree

data Const = Char Char | Num Double

data Func = Sin | Cos | Tg | Exp | Ln | Pow Double
    deriving Eq

data Var = X | Y | Z
    deriving Eq

data ListOp = Add | Mul
    deriving Eq

data TwoArgOp = Div | Substr
    deriving Eq

-- zdefiniowanie precedensów operatorów
class Op op where
    precedence :: op -> Integer

instance Op ListOp where precedence = precedenceListOp
instance Op TwoArgOp where precedence = precedenceTwoArg

precedenceListOp :: ListOp -> Integer
precedenceListOp Mul = 2
precedenceListOp Add = 1
precedenceTwoArg :: TwoArgOp -> Integer
precedenceTwoArg Div      = 3
precedenceTwoArg Substr   = 1