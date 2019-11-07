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
  where f x = if (x < to && step > 0) || (x > to && step < 0) then Just (x, x + step) else Nothing

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

-- Test
testSum lst exptected = exptected == sum lst
testSums = [testSum [1,2,3] 6, testSum [] 0, testSum [-1,-2,-3,-4] (-10)]

testProduct lst expected = expected == product lst
testProducts = [testProduct [1,2,3] 6, testProduct [] 1, testProduct [0,1,2] 0]

testFilterPositive lst expected = expected == filterNot (\x -> x <= 0) lst
testFilterPositives = [testFilterPositive [-1,2,3] [2,3], testFilterPositive [-2,-3] [], testFilterPositive [1,2] [1,2]]

testReverse lst = (reverse . reverse) lst == lst
testReverses = [testReverse [1,2,3,4], testReverse [1]]

testRange f t s expected = expected == rangeTo f t s
testRanges = [testRange 1 10 3 [1,4,7], testRange (-5) 6 5 [-5,0,5], testRange 10 1 1 [], testRange 10 7 (-2) [10,8]]

testGroup lst n exptected = exptected == groups lst n
testGroups = [testGroup [1,2,3,4] 3 [[1,2,3],[4]], testGroup [1,2,3] 4 [[1,2,3]], testGroup [1,2,3] 1 [[1],[2],[3]]]

testAll = map (\b -> if b then "OK" else "FAIL") $
  Prelude.concatMap id [testSums, testProducts, testFilterPositives, testReverses, testRanges, testGroups]
