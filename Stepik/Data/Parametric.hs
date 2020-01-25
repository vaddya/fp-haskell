module Stepik.Data.Parametric where

import Data.Char(isDigit)

{-
  Реализуйте функции distance, считающую расстояние между двумя точками с вещественными координатами, 
  и manhDistance, считающую манхэттенское расстояние между двумя точками с целочисленными координатами.
-}
data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

{-
  Реализуйте функции getCenter, которая принимает координату ячейки и возвращает координату ее центра, 
  и функцию getCell, которая принимает координату точки и возвращает номер ячейки в которой находится данная точка. 
-}
getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord ((fromIntegral x + 0.5) * size) ((fromIntegral y + 0.5) * size)

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord (floor (x / size)) (floor (y / size))

{-
  Реализуйте функцию, которая ищет в строке первое вхождение символа, который является цифрой, 
  и возвращает Nothing, если в строке нет цифр.
-}
findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x : xs)
  | isDigit x = Just x
  | otherwise = findDigit xs

{-
  Реализуйте функцию findDigitOrX, использующую функцию findDigit: findDigitOrX должна находить цифру в строке, 
  а если в строке цифр нет, то она должна возвращать символ 'X'. Используйте конструкцию case.
-}
findDigitOrX :: [Char] -> Char
findDigitOrX str = case findDigit str of
  Just c  -> c
  Nothing -> 'X'

{-
  Реализовать функции maybeToList и listToMaybe, преобразующие Maybe a в [a] и наоборот 
  (вторая функция отбрасывает все элементы списка, кроме первого).
-}
maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList (Nothing) = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

{-
  Реализуйте функцию приведения Either к Maybe
-}
eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing