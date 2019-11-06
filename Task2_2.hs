module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Prelude hiding (foldl, foldr, map, concatMap,
  filter, reverse, sum, product, elem)
import qualified Prelude (foldl, foldr, map, concatMap,
  filter, reverse, sum, product, elem)
import qualified Data.List (unfoldr)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ s [] = s
foldl f z (x : xs) = foldl f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ s [] = s
foldr f z (x : xs) = f x $ foldr f z xs

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f z = case f z of
  Nothing -> []
  Just (a, b) -> a : unfoldr f b

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f lst = foldr f' [] lst 
  where f' x acc = f x : acc

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldr f' [] lst 
  where 
    f' Nothing acc = acc
    f' (Just v) acc = v : acc

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal mat = unfoldr f mat
  where f m = if null m then Nothing else Just (head $ head m, tail $ map tail m)

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f lst = foldr f' [] lst
  where f' x acc = if not $ f x then x : acc else acc

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem y lst = foldl f False lst
  where f b x = if b || x == y then True else False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr f from
  where f x = if x < to then Just (x, x + step) else Nothing

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr f lst
  where
    f list = if null list then Nothing else Just (take n' list, drop n' list)
    n' = fromIntegral n
