module Stepik.List.Fold where

import Data.List (unfoldr)

{-
  Напишите реализацию функции concatList через foldr
-}
concatList :: [[a]] -> [a]
concatList = foldr (++) []

{-
  Используя функцию foldr, напишите реализацию функции lengthList, вычисляющей количество элементов в списке.
-}
lengthList :: [a] -> Int
lengthList = foldr (const (+1)) 0 

{-
  Реализуйте функцию sumOdd, которая суммирует элементы списка целых чисел, имеющие нечетные значения
-}
sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

{-
  Реализуйте функцию meanList, которая находит среднее значение элементов списка, 
  используя однократный вызов функции свертки.
-}
meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (s, c) -> (s + x, c + 1)) (0, 0)

{-
  Используя однократный вызов свертки, реализуйте функцию evenOnly, которая выбрасывает 
  из списка элементы, стоящие на нечетных местах, оставляя только четные.
  Попробуйте добиться того, чтобы функция позволяла работать и с бесконечными списками.
-}
evenOnly :: [a] -> [a]
evenOnly xs = foldr (\(x, i) s -> if odd i then s else x : s) [] (zip xs [1..])

{-
  Напишите реализацию функции, возвращающей последний элемент списка, через foldl1.
-}
lastElem :: [a] -> a
lastElem = foldl1 (flip const)

{-
  Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке 
  список символов, попадающих в заданный парой диапазон. 
-}
revRange :: (Char, Char) -> [Char]
revRange (a, b) = unfoldr g b
  where
    g :: Char -> Maybe (Char, Char)
    g x = if x >= a then Just(x, pred x) else Nothing
