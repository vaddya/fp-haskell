module Stepik.Intro.Functions where

{-
  Реализуйте функцию трех аргументов lenVec3, которая вычисляет длину трехмерного вектора. 
  Аргументы функции задают декартовы координаты конца вектора, 
  его начало подразумевается находящимся в начале координат. 
-}
lenVec3 :: Floating a => a -> a -> a -> a
lenVec3 x y z = sqrt x ^ 2 + y ^ 2 + z ^ 2

{-
  Напишите реализацию функции sign, которая возвращает 
  1, если ей передано положительное число, 
  (-1), если отрицательное, 
  и 0 в случае, когда передан 0.
-}
sign :: (Num a, Ord a) => a -> a
sign x = if x == 0 then 0 else if x > 0 then 1 else -1




