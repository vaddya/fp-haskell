module Stepik.List.HOF where

import Data.Char (isDigit, isLower)

{-
  Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
  Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.
-}
readDigits :: String -> (String, String)
readDigits str = helper str ""
  where
    helper :: String -> String -> (String, String)
    helper "" curr = (curr, "")
    helper xs@(x : xs') curr
      | isDigit x  = helper xs' (curr ++ [x])
      | otherwise  = (curr, xs)

{-
  Реализуйте функцию filterDisj, принимающую два унарных предиката и список, 
  и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.
-}
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x : xs)
  | p1 x || p2 x  = x : filterDisj p1 p2 xs
  | otherwise     = filterDisj p1 p2 xs

{-
  Напишите реализацию функции qsort. Функция qsort должная принимать на вход 
  список элементов и сортировать его в порядке возрастания с помощью сортировки Хоара
-}
qsort :: Ord a => [a] -> [a]
qsort xs | length xs <= 1 = xs
qsort (x : xs) = qsort (filter (< x) xs) ++ x : qsort(filter (>= x) xs)

{-
  Напишите функцию squares'n'cubes, принимающую список чисел,
  и возвращающую список квадратов и кубов элементов исходного списка.
  squares'n'cubes [3,4,5] -> [9,27,16,64,25,125]
-}
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

{-
  Воспользовавшись функциями map и concatMap, определите функцию perms, которая возвращает 
  все перестановки, которые можно получить из данного списка, в любом порядке.
-}
perms :: [a] -> [[a]]
perms xs | length xs <= 1 = [xs]
perms xs = concatMap (\(x, xs) -> map (x :) (perms xs)) (select xs)
  where
    select :: [a] -> [(a, [a])]
    select [] = []
    select (x:xs) = select' x [] xs

    select' :: a -> [a] -> [a] -> [(a, [a])]
    select' x left [] = [(x, left)]
    select' x left right@(r : rs) = (x, left ++ right) : select' r (x : left) rs

{-
  Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие из символов в верхнем регистре.
-}
delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

{-
  Напишите функцию max3, которой передаются три списка одинаковой длины и которая возвращает список той же длины, 
  содержащий на k-ой позиции наибольшее значение из величин на этой позиции в списках-аргументах.
-}
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> x `max` y `max` z)

