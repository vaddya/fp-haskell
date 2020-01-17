import Data.List(sort)

{-
  Задание 1 
  Даны два списка чисел. Верните список чисел, присутствующих в обоих исходных списках.
  Пример:
  mergeLists [0,1,2,3,4] [4,2,40] -> [2,4]
  mergeLists [0,1,2] [4,5] -> []
-}

intersect :: [Int] -> [Int] -> [Int]
intersect x y = intersect' (sort x) (sort y) []
  where
    -- first -> second -> acc -> merged
    intersect' :: [Int] -> [Int] -> [Int] -> [Int]
    intersect' [] _ acc = acc
    intersect' _ [] acc = acc
    intersect' xs'@(x : xs) ys'@(y : ys) acc
      | x == y  = intersect' xs  ys  (x : acc)
      | x <  y  = intersect' xs  ys' acc
      | x >  y  = intersect' xs' ys  acc

{-
  Задание 2
  Дан список чисел ts и число x. Верните список чисел, в которых присутствует 
  хотя бы одна такая же цифра, как в x. Систему счисления считать десятичной.
  Пример:
  sameDigits [1,11,45,23] 12 -> [1,11,23]
  sameDigits [72,47,55] 7 -> [72, 47]
-}

sameDigits :: [Int] -> Int -> [Int]
sameDigits ts x = 
  let digitsx = digits x in 
    filterNot (\y -> null $ intersect digitsx (digits y)) ts
  where
    -- number -> digits
    digits :: Int -> [Int]
    digits x
      | x == 0     = []
      | otherwise  = x `mod` 10 : digits (x `div` 10)
    
    filterNot p = filter $ not . p

{-
  Задание 3
  Реализуйте функцию, которая принимает на вход список целых чисел и возвращает 
  для каждого элемента его изменение относительно предыдущего в некотором представлении.
  Также необходимо реализовать требуемый для решения задачи тип данных.
  Пример:
  diffs [1,2,3,2,2] -> [Plus 1, Plus 1, Minus 1, Equal]
  diffs [1,32,32,1] -> [Plus 31, Equal, Equal, Minus 31]
-}

data Diff = Zero | Plus Int | Minus Int
  deriving Show

diffs :: [Int] -> [Diff]
diffs [] = []
diffs (x : xs) = reverse $ diffs' xs x []
  where
    -- list -> previous -> acc -> diffs
    diffs' :: [Int] -> Int -> [Diff] -> [Diff]
    diffs' [] p acc = acc
    diffs' (x : xs) p acc = diffs' xs x (next : acc)
      where 
        next
          | x == p  = Zero
          | x <  p  = Minus $ p - x
          | x >  p  = Plus $ x - p

{-
  Задание 4
  Постройте уравнение прямой, проходящей через две заданные точки 
  в прямоугольной декартовой системе координат на плоскости.
  Пример:
  equline 3 2 2 6 -> "4x + 1y = 14"
-}

equline x1 y1 x2 y2 = a ++ "x + " ++ b ++ "y = " ++ c
  where
    a = show $ y2 - y1
    b = show $ x1 - x2
    c = show $ x1 * y2 - x2 * y1 

{-
  Задание 5
  Опишите тип данных для реализации троичной логики со значениями "Правда", "Ложь" и "Неизвестно". 
  Реализуйте функции вычисления операций "не", "и" и "или" над этим типом.
-}

data Ternary = Truth | Unknown | Falsehood
  deriving (Show, Eq)

terNot :: Ternary -> Ternary
terNot Truth = Falsehood
terNot Falsehood = Truth
terNot Unknown = Unknown

terAnd :: Ternary -> Ternary -> Ternary
terAnd Unknown _ = Unknown
terAnd _ Unknown = Unknown
terAnd Truth Truth = Truth
terAnd _ _ =  Falsehood

terOr :: Ternary -> Ternary -> Ternary
terOr Unknown _ = Unknown
terOr _ Unknown = Unknown
terOr Falsehood Falsehood = Falsehood
terOr _ _ = Truth
