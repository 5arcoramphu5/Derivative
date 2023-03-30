module Utils.ToString () where

import Utils.DataTypes

instance Show ExprThree where show = exprToString
instance Show Func where show = funcToString
instance Show Var where show = varToString
instance Show ListOp where show = listOpToString
instance Show TwoArgOp where show = twoArgOpToString

exprToString :: ExprThree -> String

-- wypisywanie stałych i zmiennych 
exprToString (Const (Char a)) = [a]
exprToString (Const (Num a)) = show a
exprToString (Var v) = varToString v

-- wypisywanie potęg bez nawiasów dla stałych i zmiennych i z dla innych wyrażeń
exprToString (Apply (Pow n) (Var v)) = (varToString v) ++ "^" ++ (show n)
exprToString (Apply (Pow n) (Const c)) = (exprToString (Const c)) ++ "^" ++ (show n)
exprToString (Apply (Pow n) expr) = (brackets expr) ++ "^" ++ (show n)

-- wypisywanie funkcji z argumentami w nawiasach
exprToString (Apply f expr) = (funcToString f) ++ (brackets expr)

-- wypisywanie wyrażeń z operatorem nawiasując te argumenty które są wyrażeniami z operatorem o wyższym precedensie

exprToString (TwoArgOp l op r) = (bracketBasedOnPrecedence op l) ++ " " ++ (twoArgOpToString op) ++ " " ++ (bracketBasedOnPrecedence op r)

exprToString (ListOp op list) = concat ( tail (foldl 
    (\l x -> l++[opStr]++[bracketBasedOnPrecedence op x]) [] list) )
    where opStr = " " ++ (listOpToString op) ++ " "

-- FUNKCJE POMOCNICZE:

-- funkcja nawiasująca wyrażenie jeśli jest ono wyrażeniem z operatorem o większym lub równym precedensie niż operator podany jako argument
bracketBasedOnPrecedence :: Op o => o -> ExprThree -> String
bracketBasedOnPrecedence outerOp (ListOp op l)
    | precedence op < precedence outerOp    = brackets (ListOp op l)
    | True                              = exprToString (ListOp op l)
bracketBasedOnPrecedence outerOp (TwoArgOp l op r)
    | precedence op < precedence outerOp    = brackets (TwoArgOp l op r)
    | True                              = exprToString (TwoArgOp l op r)
bracketBasedOnPrecedence op expr = exprToString expr

brackets :: ExprThree -> String
brackets expr = "(" ++ (exprToString expr) ++ ")"

-- funkcje zamieniające inne typy na string
varToString :: Var -> String
varToString X = "x"
varToString Y = "y"
varToString Z = "z"

listOpToString :: ListOp -> String
listOpToString Add = "+"
listOpToString Mul = "*"

twoArgOpToString :: TwoArgOp -> String
twoArgOpToString Substr = "-"
twoArgOpToString Div = "/"

funcToString :: Func -> String
funcToString Sin = "sin"
funcToString Cos = "cos"
funcToString Tg = "tg"
funcToString Exp = "exp"
funcToString Ln = "ln"