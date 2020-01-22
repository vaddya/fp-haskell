module Task.Math where

{-
  Реализовать функции
-}

import Todo(todo)
import Prelude hiding (gcd, cos, sin)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = sum $ take 10 $ taylorSeries x (3, x)

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = sum $ take 10 $ taylorSeries x (2, 1)

-- Taylor series for sin and cos
taylorSeries :: Double -> (Double, Double) -> [Double]
taylorSeries x (n1, x1) = map snd $ iterate iter (n1, x1)
  where
    iter :: (Double, Double) -> (Double, Double)
    iter (n, prev) = (n + 2, prev * (-1) * x * x / (n - 1) / n)

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x 0 = x
gcd x y = gcd y (x `mod` y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | isLeap year && month == 3 && day == 29 = True -- leap day
  | month > 0 && month <= 12 && day > 0 && day <= daysInMonth month = True
  | otherwise = False
  where 
    isLeap :: Integer -> Bool
    isLeap year = year `mod` 4 == 0 && year `mod` 400 /= 0
    
    daysInMonth :: Integer -> Integer
    daysInMonth x
      | x == 2 = 28
      | x `elem` [1,3,5,7,8,10,12] = 31
      | otherwise = 30

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y
 | y < 0            = error "Negative exponent"
 | x == 1 || y == 0 = 1
 | y == 1           = x
 | otherwise        = x * pow x (y - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n
  | n == 2 = True
  | n == 1 || n `mod` 2 == 0 = False
  | otherwise = not $ any (\x -> n `mod` x == 0) [3, 5 .. round $ sqrt $ fromIntegral n]

