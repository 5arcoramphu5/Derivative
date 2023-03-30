module Utils.ComprExpr () where

import Utils.DataTypes

-- PORÓWNYWANIE WYRAŻEŃ
-- jest konieczne do upraszczania wyrażeń
-- zdefiniowanie porządku, żeby móc wygodnie porównywać ze sobą wyrażenia z dodawaniem i mnożeniem, które są działaniami przemiennymi
-- - są sobie równe gdy lista argumentów jednego wyrażenia jest permutacją listy argumentów drugiego 

instance Eq ExprThree where (==) = (\e1 e2 -> exprComp e1 e2 == EQ)
instance Ord ExprThree where compare = exprComp
exprComp :: ExprThree -> ExprThree -> Ordering

-- porównywanie stałych przez porównanie ich wartości
exprComp (Const (Char c1)) (Const (Char c2)) = compare c1 c2
exprComp (Const (Num n)) (Const (Num m)) = compare n m

-- porównywanie potęg po wykładnikach
exprComp (Apply (Pow n) expr1) (Apply (Pow m) expr2) = 
    if n == m then exprComp expr1 expr2
    else compare n m

-- porównywanie wyrażeń po numeracji
exprComp (Var v) (Var w) = compare (numerationV v) (numerationV w)

exprComp (Apply f expr1) (Apply g expr2) = 
    if comp == EQ then exprComp expr1 expr2
    else comp
    where comp = compare (numerationF f) (numerationF g)

exprComp (TwoArgOp expr1 op1 _) (TwoArgOp expr2 op2 _) =
    if comp == EQ then exprComp expr1 expr2
    else comp
    where comp = compare (numerationTAO op1) (numerationTAO op2)

-- porównywanie wyrażeń dodawania i mnożenia -
-- najpierw porównanie operatorów, w razie ich równości porównanie posortowanych list ich argumentów
-- jeśli są sobie równe wtedy jedna jest permutacją drugiej
exprComp (ListOp op1 l1) (ListOp op2 l2) =
    if comp == EQ 
        then compare (sort l1) (sort l2)
    else comp
    where comp = compare (numerationLO op1) (numerationLO op2)

exprComp expr1 expr2 = compare (numeration expr1) (numeration expr2)

-- SORTOWANIE - QUICKSORT
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort (filter (<= x) xs) ++ [x] ++ sort (filter (> x) xs)

-- numeracja dla wygodnego zdefiniowania porządku
numeration :: ExprThree -> Integer
numerationF :: Func -> Integer
numerationV :: Var -> Integer
numerationLO :: ListOp -> Integer
numerationTAO :: TwoArgOp -> Integer 

numeration (Const (Num _))  = 0
numeration (Const (Char _)) = 1
numeration (Var _)          = 2
numeration (Apply _ _)      = 3
numeration (ListOp _ _)     = 4
numeration (TwoArgOp _ _ _) = 5

numerationV X = 1
numerationV Y = 2
numerationV Z = 3

numerationLO Mul = 1
numerationLO Add = 2

numerationTAO Div = 1
numerationTAO Substr = 1

numerationF (Pow _) = 0
numerationF Sin     = 1
numerationF Cos     = 2
numerationF Tg      = 3
numerationF Exp     = 4
numerationF Ln      = 5