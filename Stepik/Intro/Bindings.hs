module Stepik.Intro.Bindings where

{-
  Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности
  a_0 = 1; a_1 = 2 ; a_2 = 3; a_{k+3} = a_{k+2} + a_{k+1} - 2 a_{k}.
-}
seqA :: Integer -> Integer
seqA n 
  | n < 3 = n + 1
  | otherwise = helper 1 2 3 n
    where
      helper :: Integer -> Integer -> Integer -> Integer -> Integer
      helper a b c 2 = c
      helper a b c k = helper b c (b + c - 2 * a) (k - 1)

{-
  Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.
-}
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper x (0, 0) where
  helper :: Integer -> (Integer, Integer) -> (Integer, Integer)
  helper 0 (0, 0) = (0, 1)
  helper x (s, c) =
    if x == 0 
    then (abs s, c)
    else helper (x `quot` 10) (s + x `rem` 10, c + 1)

{-
  Реализуйте функцию, находящую значение определённого интеграла 
  от заданной функции f на заданном интервале [a,b] методом трапеций.
-}
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ( 0.5 * (f a + f b) + (sum $ map f points) )
  where
    n = 1000
    h = (b-a)/n
    points = [a + h * x | x <- [1..n-1]]
