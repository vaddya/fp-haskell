module Task.Term where

import Todo (todo)

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

data Operation = Plus | Minus | Times
  deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }                          -- числовая константа
          | Variable{ varName :: String }                           -- переменная
          | BinaryTerm{ lhv :: Term, rhv :: Term, op :: Operation } -- бинарная операция
  deriving (Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
l |+| r = BinaryTerm l r Plus
infixl 6 |+|

(|-|) :: Term -> Term -> Term
l |-| r = BinaryTerm l r Minus
infixl 6 |-|

(|*|) :: Term -> Term -> Term
l |*| r = BinaryTerm l r Times
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
  case expression of
    term@(IntConstant _)    -> term
    term@(Variable name)    -> if name == varName then replacement else term
    (BinaryTerm lhv rhv op) -> BinaryTerm left right op
      where
        left  = replaceVar varName replacement lhv
        right = replaceVar varName replacement rhv

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate term@(IntConstant _)    = term
evaluate (Variable varName)      = error(varName ++ " is not a constant")
evaluate (BinaryTerm lhv rhv op) =
  case op of
    Plus  -> IntConstant $ left + right
    Minus -> IntConstant $ left - right
    Times -> IntConstant $ left * right
  where
    left  = intValue $ evaluate lhv
    right = intValue $ evaluate rhv

