module Utils.Parser (parse) where

import Utils.DataTypes 

data Operator = Func Func | LO ListOp | TAO TwoArgOp | LPar   
parse :: String -> Maybe ExprThree
shuntingYard :: String -> [ExprThree] -> [Operator] -> Maybe ExprThree

-- PARSOWANIE WYRAŻEŃ ZA POMOCĄ SHUNTING YARD ALGORITHM
-- kolejne kroki alorytmu są wykonywane przez funkcję shuntingYard przyjmująca jako argumenty:
-- string do parsowania
-- aktualną listę outputu 
-- aktualny stos operatorów
-- funkcja zwraca Nothing jeśli podany string nie zawierał poprawnego wyrażenia

parse s = shuntingYard s [] []

-- ODCZYTANIE FUNKCJI
-- dodanie jej do stosu operatorów

shuntingYard ('s':('i':('n' : s))) output operators = shuntingYard s output (Func Sin : operators)
shuntingYard ('c':('o':('s' : s))) output operators = shuntingYard s output (Func Cos : operators)
shuntingYard ('t':('g' : s)) output operators = shuntingYard s output (Func Tg : operators)
shuntingYard ('e':('x':('p' : s))) output operators = shuntingYard s output (Func Exp : operators)
shuntingYard ('l':('n' : s)) output operators = shuntingYard s output (Func Ln : operators)

-- ODCZYTANIE NAWIASÓW 
-- dodanie lewego nawiasu do stosu operatorów
-- dla prawego nawiasu:
--  zdejmowanie operatorów ze stosu do momentu napotkania lewego nawiasu i jeśli po nim na stosie jest funkcja - wtedy nałożenie jej
shuntingYard ('(' : s) output operators = shuntingYard s output (LPar : operators)

shuntingYard (')' : s) output [] = Nothing 
shuntingYard (')' : s) output operators = 
    case dropOps of
        [] -> Nothing
        _ -> (uncurry (shuntingYard s)) (popIfFunc (tail dropOps) dropOutp)
    where (dropOps, dropOutp) = popUntilLPar operators output

-- ODCZYTANIE OPERATORA 
-- ściąganie i nakładanie na output operatorów ze stosu dopóki mają większy precedens niż odczytany operator
-- dodanie go do stosu

shuntingYard ('+' : s) output operators = 
    shuntingYard s dropOutp (LO Add : dropOps) 
    where (dropOps, dropOutp) = popWhileGrPrec Add operators output

shuntingYard ('*' : s) output operators = 
    shuntingYard s dropOutp (LO Mul : dropOps) 
    where (dropOps, dropOutp) = popWhileGrPrec Mul operators output

shuntingYard ('/' : s) output operators = 
    shuntingYard s dropOutp (TAO Div : dropOps) 
    where (dropOps, dropOutp) = popWhileGrPrec Div operators output

shuntingYard ('-' : s) output operators = 
    shuntingYard s dropOutp (TAO Substr : dropOps) 
    where (dropOps, dropOutp) = popWhileGrPrec Substr operators output

-- pominiecie spacji 
shuntingYard (' ' : s) output operators = shuntingYard s output operators

-- ODCZYTANIE ZMIENNYCH 
-- dodanie ich do outputu
shuntingYard ('x' : s) output operators = shuntingYard s (Var X : output) operators
shuntingYard ('y' : s) output operators = shuntingYard s (Var Y : output) operators
shuntingYard ('z' : s) output operators = shuntingYard s (Var Z : output) operators

-- PRZYPADKI PO ODCZYTANIU WSZYSTKICH ZNAKÓW

-- poprawne zakończenie algorytmu
shuntingYard "" [expr] [] = Just expr

-- zwrócenie błędu dla niepoprawnego stringa
shuntingYard "" [] _ = Nothing
shuntingYard "" _ [] = Nothing
shuntingYard "" _ (LPar : _) = Nothing

-- nakładanie operatorów, które zostały na stosie
shuntingYard "" output (op : ops) =
    shuntingYard "" (apply op output) ops

-- ODCZYTANIE STAŁYCH
shuntingYard s output operators = 
    if inRange ('a', 'z') (head s) 
        then shuntingYard (tail s) (Const (Char (head s)) : output) operators
    else (
        if inRange ('0', '9') (head s) 
            then shuntingYard (dropWhile (inRange ('0', '9')) s) (Const (Num (parseNum (takeWhile (inRange ('0', '9')) s))) : output) operators
        else Nothing )    

--  FUNKCJE POMOCNICZE

parseNum :: String -> Double
parseNum s = fst (foldl 
    (\(curr, pow) c -> (curr + pow * (fromIntegral ((fromEnum c) - (fromEnum '0'))), pow * 10)) 
    (0, 1) (reverse s))

-- sprawdzenie czy znak znajduje się w podanym zakresie
inRange :: (Char, Char) -> Char -> Bool
inRange (a, b) c = (fromEnum c) >= (fromEnum a) && (fromEnum c) <= (fromEnum b) 

-- zdejmowanie i nakładanie na output operatorów ze stosu dopóki mają większy precedens od podanego operatora
popWhileGrPrec :: Op o => o -> [Operator] -> [ExprThree] -> ([Operator], [ExprThree])

popWhileGrPrec op1 ((LO op) : ops) output =
    if precedence op > precedence op1 
        then popWhileGrPrec op1 ops (apply (LO op) output)
    else ( ((LO op) : ops) , output) 

popWhileGrPrec op1 ((TAO op) : ops) output =
    if precedence op > precedence op1 
        then popWhileGrPrec op1 ops (apply (TAO op) output)
    else ( ((TAO op) : ops) , output) 

popWhileGrPrec op operators output = (operators, output)

-- zdjejmowanie i nakładanie na output operatorów ze stosu do momentu dodarcia do lewego nawiasu
popUntilLPar :: [Operator] -> [ExprThree] -> ([Operator], [ExprThree])
popUntilLPar [] output = ([], output)
popUntilLPar (LPar : ops) output = ((LPar : ops), output)
popUntilLPar (op : ops) output = popUntilLPar ops (apply op output)
    
-- nałożenie operatora na argumenty z outputu i dodanie wyniku do outputu
apply :: Operator -> [ExprThree] -> [ExprThree]
apply (Func f) (arg : exprs) = (Apply f arg : exprs)
apply (LO op) (arg2 : (arg1 : exprs)) = (ListOp op [arg1, arg2] : exprs)
apply (TAO op) (arg2 : (arg1 : exprs)) = (TwoArgOp arg1 op arg2 : exprs)
apply _ _ = []

-- nałożenie operatora z wierzchu stosu jeśli jest funkcją
popIfFunc :: [Operator] -> [ExprThree] -> ([ExprThree], [Operator])
popIfFunc (Func f : ops) (arg : exprs) = ( ((Apply f arg):exprs) , ops)
popIfFunc operators output = (output, operators) 