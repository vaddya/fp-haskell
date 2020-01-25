module Stepik.Basic.Class where

{-
  Реализуйте класс типов Printable, предоставляющий один метод toString — 
  функцию одной переменной, которая преобразует значение типа, являющегося 
  представителем Printable, в строковое представление.
  Сделайте типы данных Bool и () представителями этого класса типов.
-}

class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString x = if x then "true" else "false"

instance Printable () where
  toString x = "unit type"

{-
  Сделайте тип пары представителем класса типов Printable, реализованного вами в предыдущей задаче.
-}
instance (Printable a, Printable b) => Printable (a, b) where
  toString (x, y) = "(" ++ toString x ++ "," ++ toString y ++ ")"


{-
  Задайте реализацию по умолчанию метода stompOrStab, которая вызывает метод stomp, если переданное ему 
  значение приводит в ярость Морка; вызывает stab, если оно приводит в ярость Горка и вызывает сначала stab, 
  а потом stomp, если оно приводит в ярость их обоих. Если не происходит ничего из вышеперечисленного, 
  метод должен возвращать переданный ему аргумент.
-}
class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab x | doesEnrageGork x && doesEnrageMork x = stomp (stab x)
                | doesEnrageGork x = stab x
                | doesEnrageMork x = stomp x
                | otherwise = x

{-
  Имея функцию ip = show a ++ show b ++ show c ++ show d определите значения a, b, c, d так, 
  чтобы добиться следующего поведения: ip -> "127.224.120.12"
  Sic!
-}
a = 127.22
b = 4.1
c = 20.1
d = 2

{-
  Реализуйте класс типов SafeEnum, обе функции которого ведут себя как succ и pred 
  стандартного класса Enum, однако являются тотальными.
-}
class (Eq a, Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x = if x == maxBound then minBound else succ x

  spred :: a -> a
  spred x = if x == minBound then maxBound else pred x
  
{-
  Напишите функцию с сигнатурой:
  avg :: Int -> Int -> Int -> Double
  вычисляющую среднее значение переданных в нее аргументов
-}
avg :: Int -> Int -> Int -> Double
avg a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3
