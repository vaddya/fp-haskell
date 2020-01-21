module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

data Operation = Plus | Minus | Times
  deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }                          -- числовая константа
          | Variable{ varName :: String }                           -- переменная
          | BinaryTerm{ lhv :: Term, rhv :: Term, op :: Operation } -- бинарная операция
  deriving(Show,Eq)

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

-- Test replace variable
testReplace :: Int -> String -> Term -> Term -> Bool
testReplace expected varName replacement expression = actual == expected
  where actual = (intValue . evaluate) $ replaceVar varName replacement expression

testReplace1 = testReplace 8 "test" (IntConstant 4) (IntConstant 2 |*| Variable "test")
testReplace2 = testReplace 2 "test" (IntConstant 0) (IntConstant 2 |+| Variable "test" |*| IntConstant 100)
testReplace3 = testReplace 5 "test" (IntConstant 5) (IntConstant 5)

-- Test evaluate term
testEval :: Int -> Term -> Bool
testEval expected term = actual == expected
  where actual = (intValue . evaluate) term

testEval1 = testEval 8 (IntConstant 4 |*| (IntConstant 4 |-| IntConstant 1 |*| IntConstant 2))
testEval2 = testEval 6 (IntConstant 1 |+| IntConstant 3 |*| IntConstant 5 |-| IntConstant 10)
testEval3 = testEval 5 (IntConstant 5)

testAll = map (\b -> if b then "OK" else "FAIL")
  [testReplace1, testReplace2, testReplace3, testEval1, testEval2, testEval3]
