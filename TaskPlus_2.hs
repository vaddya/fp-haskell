import Todo(todo)
import Data.List(sort)

-- Задание 1
-- Дан список целых чисел и два числа m и n. Заменить в списке все элементы, кратные
-- m на n.
-- Пример:
-- replace [1, 2, 3, 4, 9] 3 11
-- [1, 2, 11, 4, 11]

replace :: [Int] -> Int -> Int -> [Int]
replace lst m n = map (\x -> if x `mod` m == 0 then n else x) lst

-- Задание 2
-- Дано два целых числа m и n. Найти сумму всех целых чисел, лежащих в диапазоне [m, n)
-- Пример:
-- intervalSum 3 5
-- 7

intervalSum :: Int -> Int -> Int
-- intervalSum m n = sum [m .. (n - 1)]
intervalSum m n = intervalSum' m n 0
  where
    -- from -> to -> acc -> sum
    intervalSum' :: Int -> Int -> Int -> Int
    intervalSum' from to acc
      | from == to  = acc
      | otherwise   = intervalSum' (from + 1) to (from + acc)

-- Задание 3
-- Дан список целых чисел. Определить, имеются ли в данном списке одинаковые числа
-- идущие подряд друг за другом. Вернуть индекс первого повторяющегося числа или -1,
-- если повторов нет

duplicateIndex :: [Int] -> Int
duplicateIndex [] = (-1)
duplicateIndex (x : xs) = duplicateIndex' xs x 0
  where
    -- list -> previous -> index -> result
    duplicateIndex' :: [Int] -> Int -> Int -> Int
    duplicateIndex' [] _ _ = (-1)
    duplicateIndex' (x : xs) p i =
      if x == p then i + 1 else duplicateIndex' xs x (i + 1)

-- Задание 4
-- Дан список целых чисел. Вернуть новый список, в котором каждый элемент 
-- оригинального списка уменьшен на медиану этого списка. Медианой списка является
-- элемент, стоящий посередине упорядоченного по возрастанию списка (если элементов
-- четное количество, медиана считается как средне арифметическое двух средних 
-- элементов).

center :: [Int] -> [Int]
center lst = map (\x -> x - median) lst
  where
    len = length lst
    sorted = sort lst
    idx = len `div` 2
    median =
      if len `mod` 2 == 1
      then sorted !! idx
      else (sorted !! idx + sorted !! idx - 1) `div` 2

-- Задание 5
-- Дана база данных о продажах некоторого интернет-магазина. Каждая строка входного
-- файла представляет собой запись вида:
-- Покупатель товар цена дата
-- Покупатель — имя покупателя (строка без пробелов), товар — название товара (строка
-- без пробелов), цена - стоимость товара (целое число), дата - дата покупки в 
-- формате XX.XX.XXXX
--
-- Для каждого человека посчитайте сколько денег он потратил за указанный в базе 
-- период времени. Результат верните в виде списка, отсортированного по убыванию 
-- общей потраченной суммы 
--
-- Пример:
-- Sergey sneakers 13000 15.01.2018
-- Kirill mobile 15000 16.01.2018
-- Sergey headphones 10000 01.02.2018
--
-- Вывод:
-- Sergey 23000
-- Kirill 15000

-- (прототип функции можно изменить)
moneySpent :: [String] -> [(String, Int)]
moneySpent _ = todo
