module Stepik.Data.Product where

import Stepik.Data.Sum (SomeData, doSomeWork, Result(Success, Fail))

{-
  Реализуйте функцию distance, возвращающую расстояние между двумя точками.
-}
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

{-
  Реализуйте функцию area, возвращающую площадь фигуры.
-}
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b

{-
  Используя функцию doSomeWork, определите функцию doSomeWork' так, 
  чтобы она возвращала код ошибки только в случае неудачи. 
-}
data Result' = Result' Int | None

instance Show Result' where
    show (Result' x) = "Fail: " ++ show x
    show (None)      = "Success"

doSomeWork' :: SomeData -> Result'
doSomeWork' x =
  case doSomeWork x of
    (Success, _) -> None
    (Fail, c)    -> Result' c

{-
  Реализуйте функцию isSquare, проверяющую является ли фигура квадратом.
-}
square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle x y) = x == y
isSquare _ = False

{-
  Целое число можно представить как список битов со знаком.
  Реализуйте функции сложения и умножения для таких целых чисел, считая, 
  что младшие биты идут в начале списка, а старшие — в конце. 
  Можно считать, что на вход не будут подаваться числа с ведущими нулями. 
-}
data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

b2i :: Bit -> Int
b2i Zero  = 0
b2i One   = 1

i2b :: Int -> Bit
i2b 0  = Zero
i2b 1  = One

s2i :: Sign -> Int
s2i Plus   = 1
s2i Minus  = -1

i2s :: Int -> Sign
i2s x | x < 0   = Minus
i2s x | x >= 0  = Plus

z2i :: Z -> Int
z2i (Z s b) = (*) (s2i s) (sum $ zipWith (*) (map b2i b) [2 ^ i | i <- [0..]])

i2z :: Int -> Z
i2z x = Z (i2s x) (map i2b $ i2bin' x)

i2bin' :: Int -> [Int]
i2bin' = reverse . i2bin

i2bin :: Int -> [Int]
i2bin n | n == 0          = []
        | n < 0           = i2bin (-n)
        | n `mod` 2 == 1  = i2bin (n `div` 2) ++ [1]
        | n `mod` 2 == 0  = i2bin (n `div` 2) ++ [0]

add :: Z -> Z -> Z
add a b = i2z (z2i a + z2i b)

mul :: Z -> Z -> Z
mul a b = i2z (z2i a * z2i b)
