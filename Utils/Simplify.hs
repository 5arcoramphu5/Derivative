module Utils.Simplify (simplify, simplStep) where

import Utils.DataTypes
import Utils.ToString
import Utils.ComprExpr

simplify :: ExprThree -> ExprThree
simplStep :: ExprThree -> ExprThree

-- funkcja upraszczająca wyrażenie dopóki wyrażenie się przez to zmienia
simplify expr = 
    if (show expr) == (show nextStep) 
    then expr
    else simplify nextStep 
    where nextStep = simplStep expr

-- wyjęcie z wyrażenia z operatorem od listy gdzie na liście jest tylko jedno wyrażenie
simplStep (ListOp op [expr]) = simplStep expr

-- upraszczanie mnożenia
-- jeśli zawiera 0 - zredukowanie wyrażenia do 0
-- uproszczenie za pomocą uogólnionej funkcji
simplStep (ListOp Mul l) =
    if elem (Const (Num 0)) l then (Const (Num 0))
    else  simplListOp Mul l 1 (\(expr, n) -> Apply (Pow n) expr)

-- uproszczenie dodawania
-- uproszcenie za pomocną uogólnionej funkcji
-- zebranie z listy argumentów list argumentów mnożeń i singletony dla innych wyrażeń
-- wyszukanie wyrażenia, które powtarza się w nich najwięcej razy
-- jesli istnieje takie powtarzające się więcej niż raz - wyjęcie przed nawias 
simplStep (ListOp Add l) = 
    if repeating == []
        then simpl
    else extrSimTerms muls (fst (maxBySnd repeating))
    where 
        simpl = simplListOp Add l 0 (\(expr, n) -> ListOp Mul [(Const (Num n)), expr])
        muls = mulLists simpl
        repeating = (filter ((>1).snd) (equivClasses (concat muls)))

-- zamiana odejmowania na dodawanie liczby ujemnej, żeby wliczało się do upraszczania listy argumentów dodawania
simplStep (TwoArgOp l Substr r) = ListOp Add [simplStep l, ListOp Mul [Const (Num (-1)), simplStep r]]

-- upraszczanie wyrażeń wewnętrznych
simplStep (Apply f expr) = Apply f (simplStep expr)

simplStep (TwoArgOp (Const (Num 0)) Div _) = (Const (Num 0)) 
simplStep (TwoArgOp l op r) = TwoArgOp (simplStep l) op (simplStep r)

simplStep expr = expr

-- FUNKCJE POMOCNICZE

-- zebranie list argumentów mnożenia z listy wyrażeń
mulLists :: ExprThree -> [[ExprThree]]
mulListsH :: ExprThree -> [ExprThree]
mulLists (ListOp Add l) = map mulListsH l
mulLists _ = []
mulListsH (ListOp Mul l) = l
mulListsH expr = [expr]

-- wyjęcie wyrażenia przed nawias:
-- dodanie wyrażeń z listy zawierających term i mnożenia temu i wyrażeń zawierających term z nim usuniętym
extrSimTerms :: [[ExprThree]] -> ExprThree -> ExprThree
extrSimTerms muls term = 
        ListOp Add 
        ((map (\x -> ListOp Mul x) (filter (not.(elem term)) muls) ) ++
        [ ListOp Mul
            [   term,
                ListOp Add (map (\l -> ListOp Mul (del term l)) (filter (elem term) muls))
            ] 
        ])

-- para o maksymalnym drugim elemencie
maxBySnd :: [(a, Int)] -> (a, Int)
maxBySnd (x:xs) = foldl (\(a, n) (b, m) -> if m > n then (b, m) else (a, n)) x xs

del :: Eq a => a -> [a] -> [a]
del x l = (tail (filter (x==) l)) ++ (filter (x/=) l)

-- uogólnione upraszczania dla mnożenia i dodawania
-- spłaszcza listy argumentów zagnieżdżonych takich samych operatorów (z łączności)
-- usuwa z nich elementy neuralne
-- łączy takie same wyrażenia za pomocą podanej funkcji od pary (wyrażenie, ilość wystąpień) (dla dodawania - mnożenie, dla mnożenia - potęgowanie)
simplListOp :: ListOp -> [ExprThree] -> Double -> ((ExprThree, Double) -> ExprThree) -> ExprThree
simplListOp op l neuElem mergeAction = 
    if merged == [] 
        then Const (Num neuElem)
    else 
        ListOp op (map simplStep merged) 
    where 
        unnested = simplNestedLists op l
        filtered = filter (/= (Const (Num neuElem))) unnested
        merged = merge filtered mergeAction 

-- połączenie wielu wystąpień wyrażenia
merge :: [ExprThree] -> ((ExprThree, Double) -> ExprThree) -> [ExprThree]
merge l mergeAction = map (\(expr, n) -> if n == 1 then expr else mergeAction (expr, fromIntegral n)) (equivClasses l)

-- uproszczenie zagnieżdżonych list argumentów
simplNestedLists :: ListOp -> [ExprThree] -> [ExprThree]
nLHelper :: ListOp -> ExprThree -> [ExprThree]

simplNestedLists op l = concatMap (nLHelper op) l

nLHelper op2 (ListOp op l) = 
    if op == op2
        then simplNestedLists op2 l
    else [ListOp op l]
nLHelper _ expr = [expr]

-- funkcja dla listy zwraca klasy równoważności elementów jako listę par - reprezentanta i ilości wystąpień równoważnych elementów
equivClasses :: Ord a => [a] -> [(a, Int)]
equivClasses [] = []
equivClasses (x:xs) = [(x, 1 + length (filter (x==) xs))] ++ (equivClasses (filter (x/=) xs)) 
