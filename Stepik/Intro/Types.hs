module Stepik.Intro.Types where

import Data.Char

{-
  Запишите тип функции standardDiscount, определенной как частичное применение функции discount:
-}
discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

{-
  Реализуйте функцию twoDigits2Int, которая принимает два символа и возвращает число, 
  составленное из этих символов, если оба символа числовые, и 100 в противном случае.
-}
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100


{-
  Реализуйте функцию dist, которая возвращает расстояние между двумя точками, 
  передаваемыми ей в качестве аргументов.
-}
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2
