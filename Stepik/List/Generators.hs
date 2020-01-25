module Stepik.List.Generators where

{-
  Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.
-}
fibStream :: [Integer]
fibStream = [0, 1] ++ zipWith (+) fibStream (tail fibStream)

{-
  Предположим, что функция repeat, была бы определена следующим образом:
  repeat = iterate repeatHelper
  определите, как должна выглядеть функция repeatHelper.
-}
repeatHelper x = x


{-
  Пусть задан тип Odd нечетных чисел следующим образом. Сделайте этот тип представителем класса типов Enum.
-}
data Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
  toEnum x = Odd(toInteger(x))
  fromEnum (Odd x) = fromIntegral(x)
  enumFrom (Odd x) = map (Odd) [x, x+2..]
  enumFromThen (Odd x) (Odd y) = map (Odd) [x, y..]
  enumFromTo (Odd x) (Odd y) = map (Odd) [x, x+2..y]
  enumFromThenTo (Odd x) (Odd y) (Odd z) = map (Odd) [x, y..z]
  succ (Odd x) = Odd(x + 2)
  pred (Odd x) = Odd(x - 2)

{-
  Пусть есть список положительных достоинств монет coins, отсортированный по возрастанию. 
  Воспользовавшись механизмом генераторов списков, напишите функцию change, которая разбивает 
  переданную ей положительную сумму денег на монеты достоинств из списка coins всеми возможными способами.
-}
coins = [2, 3, 7]
change :: Int -> [[Int]]
change n
    | n < 0      = []
    | n == 0     = [[]]
    | otherwise  = [x : y | x <- coins, y <- change(n - x)]