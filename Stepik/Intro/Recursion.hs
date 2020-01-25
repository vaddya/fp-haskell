module Stepik.Intro.Recursion where

{-
  Определите функцию, вычисляющую двойной факториал, то есть произведение натуральных чисел, 
  не превосходящих заданного числа и имеющих ту же четность.
  
  7!! -> 7 * 5 * 3 * 1
  8!! -> 8 * 6 * 4 * 2 
-}
doubleFact :: Integer -> Integer
doubleFact n = if n <= 0 then 1 else n * doubleFact(n - 2)

{-
  Измените определение функции fibonacci так, чтобы она была определена для всех 
  целых чисел и порождала при отрицательных аргументах указанную последовательность.
  
  F (−1) -> 1
  F (−2) -> −1
  F (−10) -> −55
-}
fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | abs n == 1 = 1
            | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

{-
  С помощью механизма аккумуляторов попробуйте написать более эффективную реализацию
  функции для вычисления числа Фибоначчи
-}
fibonacci' :: Integer -> Integer
fibonacci' n = helper 0 1 n
  where 
    helper :: Integer -> Integer -> Integer -> Integer
    helper curr prev n 
      | n == 0 = curr
      | n > 0 = helper (curr + prev) curr (n - 1)
      | n < 0 = helper prev (curr - prev) (n + 1)
