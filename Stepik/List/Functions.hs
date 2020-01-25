module Stepik.List.Functions where

{-
  Реализуйте функцию addTwoElements, которая бы добавляла два переданных ей значения в голову переданного списка.
  addTwoElements 2 12 [85,0,6] -> [2,12,85,0,6]
-}
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y list = x : y : list

{-
  Реализуйте функцию nTimes, которая возвращает список, состоящий из повторяющихся значений ее первого аргумента.
-}
nTimes:: a -> Int -> [a]
nTimes x 0 = []
nTimes x n = x : nTimes x (n - 1)

{-
  Сформируйте список целых чисел, содержащий только те элементы исходного списка, значение которых нечетно.
-}
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs)
  | odd x     = x : oddsOnly xs
  | otherwise = oddsOnly xs

{-
  Реализуйте функцию isPalindrome, которая определяет, является ли переданный ей список палиндромом.
-}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (x : xs)
  | null xs      = True
  | x == last xs = isPalindrome (init xs)
  | otherwise    = False

{-
  Составьте список сумм соответствующих элементов трех заданных списков.
  Длина результирующего списка должна быть равна длине самого длинного из заданных списков, 
  при этом «закончившиеся» списки не должны давать вклада в суммы.
-}
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 a b c = (head' a) + (head' b) + (head' c) : sum3 (tail' a) (tail' b) (tail' c)
  where
    head' :: (Num a) => [a] -> a
    head' [] = 0
    head' (x : _) = x
    
    tail' :: [a] -> [a]
    tail' [] = []
    tail' (_ : xs) = xs

{-
  Напишите функцию groupElems которая группирует одинаковые элементы в списке (если они идут подряд) 
  и возвращает список таких групп.
-}
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x : xs) = grouping xs [x] []
  where
    grouping :: (Eq a) => [a] -> [a] -> [[a]] -> [[a]]
    grouping list curr acc
      | null list               = acc ++ [curr]
      | head list == head curr  = grouping (tail list) (head list : curr) acc
      | otherwise               = grouping (tail list) ([head list]) (acc ++ [curr])
