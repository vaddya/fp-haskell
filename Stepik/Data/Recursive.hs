module Stepik.Data.Recursive where

{-
  Тип List, определенный ниже, эквивалентен определению списков из стандартной библиотеки в том смысле, 
  что существуют взаимно обратные функции, преобразующие List a в [a] и обратно.
-}
data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x : xs) = Cons x (toList xs)

{-
  Реализуйте функции сложения и умножения этих чисел, а также функцию, вычисляющую факториал.
-}
data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc $ toNat $ n - 1

add :: Nat -> Nat -> Nat
add x y = toNat $ fromNat x + fromNat y

mul :: Nat -> Nat -> Nat
mul x y = toNat $ fromNat x * fromNat y

fac :: Nat -> Nat
fac Zero = Suc Zero
fac a'@(Suc a) = fac a `mul` a'

{-
  Реализуйте функцию height, возвращающую высоту дерева, и функцию size, возвращающую количество узлов 
  в дереве (и внутренних, и листьев). Считается, что дерево, состоящее из одного листа, имеет высоту 0.
-}
data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node l r) = 1 + height l `max` height r

size :: Tree a -> Int
size (Leaf _) = 1
size (Node l r) = 1 + size l + size r

{-
  Теперь нам нужно написать функцию avg, которая считает среднее арифметическое всех значений в дереве. 
  И мы хотим, чтобы эта функция осуществляла только один проход по дереву. 
-}
avg :: Tree Int -> Int
avg t = uncurry (div) $ go t
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (1, x)
    go (Node l r) = (c1 + c2, s1 + s2)
      where
        (c1, s1) = go l
        (c2, s2) = go r

{-
  Исправьте определение функции expand так, чтобы она, используя дистрибутивность 
  (а также, возможно, ассоциативность и коммутативность), всегда возвращала значение, 
  эквивалентное данному и являющееся суммой произведений числовых значений.
-}
infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) =
  expand (expand e1 :*: expand e) :+:
  expand (expand e2 :*: expand e)
expand (e :*: (e1 :+: e2)) =
  expand (expand e :*: expand e1) :+:
  expand (expand e :*: expand e2)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = let
    ee1 = expand e1
    ee2 = expand e2
  in if ee1 == e1 && ee2 == e2 then e1 :*: e2 else expand $ ee1 :*: ee2
expand e = e
