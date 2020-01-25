module Task.Test4 where

import Prelude hiding(Left, Right)
import Data.List(tails)

{-
  Дана пара отсортированных списков чисел lst1 и lst2. Необходимо вернуть отсортированный 
  список, образованный объединением lst1 и lst2. Функцию sort использовать нельзя.
  Пример: merge [1,2,4,5] [2,3,4] -> [1,2,2,3,4,4,5]
-}

merge :: [Int] -> [Int] -> [Int]
merge [] lst2 = lst2
merge lst1 [] = lst1
merge xs'@(x : xs) ys'@(y : ys)
  | x < y      = x : merge xs  ys'
  | otherwise  = y : merge xs' ys

{-
  Реализуйте тип данных, позволяющий задавать команды управления лентой вида 
  "Влево на определённое число шагов", "Вправо на определённое число шагов", "Запись". 
  Реализуйте функцию, которая принимает на вход список команд и начальную позицию и 
  возвращает список позиций, на которых происходила запись.
  Пример: writePoints [Left 5, Write, Right 7, Left 1, Write] 1 -> [-4, 2]
-}

data Command = Left Int | Right Int | Write
  deriving Show

writePoints :: [Command] -> Int -> [Int]
writePoints cmds p = reverse $ writePoints' cmds p []
  where
    -- commands -> acc -> point -> write points
    writePoints' :: [Command] -> Int -> [Int] -> [Int]
    writePoints' [] _ acc = acc
    writePoints' (c : cs) p acc = case c of
      Left n  -> writePoints' cs (p - n) acc
      Right n -> writePoints' cs (p + n) acc
      Write   -> writePoints' cs p (p : acc)

{-
  Дан список чисел. Необходимо посчитать число чётных и нечётных чисел в этом списке.
  Пример: evenAndOdd [1,2,3,4,5] -> (2,3)
-}

evenAndOdd :: [Int] -> (Int, Int)
evenAndOdd lst = foldr f (0, 0) lst
  where
    f x (e, o) =
      if x `mod` 2 == 0
      then (e + 1, o)
      else (e, o + 1)

{-
  Дан список чисел и функция f :: Int -> Int -> Int. 
  Необходимо найти такую пару чисел (a,b) в списке, что f a b максимально.
  Примеры:
  maxBy (+) [] -> Nothing
  maxBy (+) [1] -> Nothing
  maxBy (+) [1,2,3] -> Just (2,3)
  maxBy (+) [1,2,3,3] -> Just (3,3)
-}

maxBy :: (Int -> Int -> Int) -> [Int] -> Maybe (Int, Int)
maxBy f lst = maxBy' (uncurry f) Nothing $ toPairs lst
  where
    -- list -> pairs
    toPairs :: [a] -> [(a, a)]
    toPairs lst = [(x, y) | (x : ys) <- tails lst, y <- ys]
    
    -- function -> current -> pairs -> max
    maxBy' :: ((Int, Int) -> Int) -> Maybe (Int, Int) -> [(Int, Int)] -> Maybe (Int, Int)
    maxBy' f current [] = current
    maxBy' f Nothing (p : ps) = maxBy' f (Just p) ps
    maxBy' f (Just c) (p : ps) = maxBy' f (Just m) ps
      where m = if f p > f c then p else c

{-
  Дан список чисел, число n и функция f :: Int -> Int -> Int. 
  Необходимо найти любую такую пару чисел (a,b) в списке, что f a b равно n.
  Примеры: 
  findBy (+) 4 [1,2,4] -> Nothing
  findBy (+) 4 [1,2,4,0] -> Just (4,0)
-}

findBy :: (Int -> Int -> Int) -> Int -> [Int] -> Maybe (Int, Int)
findBy f n lst = case [(x, y) | (x : ys) <- tails lst, y <- ys, f x y == n] of
  (x : xs)  -> Just x
  otherwise -> Nothing

{-
  Даны два списка чисел, a и b. Нужно посчитать произведение 
  всех попарных сумм элементов из a и b.
  Пример: productSum [1,2,3] [1,2] -> 2 * 3 * 4 * 3 * 4 * 5 -> 1440
-}

productSum :: [Int] -> [Int] -> Int
productSum a b = product $ [x + y | x <- a, y <- b]

{-
  Дано число n и функция f :: a -> a.
  Необходимо вернуть функцию, которая применяет f к своему аргументу n раз.
  Примеры:
  applyN (+1) 1 $ 3 -> 4
  applyN (+1) 5 $ 0 -> 5
-}

applyN :: (a -> a) -> Int -> (a -> a)
applyN f n
  | n <  1  = error("n < 1")
  | n == 1  = f
  | n >  1  = \x -> applyN f (n - 1) $ f x

{-
  Дан список функций fs :: [Int -> Int] и число x.
  Необходимо применить все функции из fs к x в порядке встречаемости в fs.
  Примеры:
  applyAll [] 42 -> 42
  applyAll [(+2), (*2)] 2 -> (2 + 2) * 2 -> 8
-}

applyAll :: [Int -> Int] -> Int -> Int
applyAll [] x = x
applyAll (f : fs) x = applyAll fs $ f x
