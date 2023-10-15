module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|
(|+|) :: Term -> Term -> Term
(|+|) a b = BinaryTerm Plus a b
infixl 6 |-|
(|-|) :: Term -> Term -> Term
(|-|) a b = BinaryTerm Minus a b
infixl 7 |*|
(|*|) :: Term -> Term -> Term
(|*|) a b = BinaryTerm Times a b

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
    IntConstant(_) -> expression
    Variable(name) -> if (name == varName) then replacement else expression
    BinaryTerm op lhv rhv -> BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm op (IntConstant a) (IntConstant b)) = case op of
    Plus -> IntConstant (a + b)
    Minus -> IntConstant (a - b)
    Times -> IntConstant (a * b)
evaluate (BinaryTerm op a@(BinaryTerm _ _ _) b@(BinaryTerm _ _ _)) = evaluate ((BinaryTerm op (evaluate a) (evaluate b)))
evaluate t = t
