module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)
import qualified Prelude (gcd, cos, sin)

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

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea (points) = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo

-- Test sin, cos, gcd, pow
cmp :: (Ord a, Num a) => (a -> a) -> (a -> a) -> a -> a -> Bool
cmp f g e x = abs (f x - g x) <= e

cmpSin = cmp sin Prelude.sin 1e-3
cmpSins = [cmpSin (-1), cmpSin 0, cmpSin 1, cmpSin 3, cmpSin 6]

cmpCos = cmp cos Prelude.cos 1e-3
cmpCoss = [cmpCos (-1), cmpCos 0, cmpCos 1, cmpCos 3, cmpSin 6]

cmpGcd x y = gcd x y == Prelude.gcd x y
cmpGcds = [cmpGcd 15 5, cmpGcd 42 56, cmpGcd (-42) 56, cmpGcd 0 1, cmpGcd 1701 3768]

cmpPow x n = pow x n == x ^ n
cmpPows = [cmpPow 3 5, cmpPow 5 3, cmpPow 0 1, cmpPow 1 0, cmpPow 2 100]

testAll = map (\b -> if b then "OK" else "FAIL") $
  concatMap id [cmpSins, cmpCoss, cmpGcds, cmpPows]
