module Task.Test1 where

import Todo(todo)
import Data.List(sortBy)
import Data.Map as Map

{-
  Задание 1
  В заданном списке целых чисел заменить каждый элемент на сумму текущего элемента 
  и всех предыдущих.
  Пример:
  accumulate [1, 2, 3, 4, 5] -> [1, 3, 6, 10, 15]
-}

accumulate :: [Int] -> [Int]
accumulate = scanl1 (+)

{-
  Задание 2
  Поменять порядок цифр в заданном положительном числе на обратный.
  Пример:
  revert 12345 -> 54321
-}

revert :: Int -> Int
revert n = revert' n 0
  where
    -- number -> acc -> reverted
    revert' :: Int -> Int -> Int
    revert' 0 acc = acc
    revert' n acc = revert' (n `div` 10) (acc * 10 + n `mod` 10)

{-
  Задание 3
  Разложить заданное целое число на простые множители. Вернуть множители в виде
  отсортированного по возрастанию списка.
-}

factorize :: Int -> [Int]
factorize n = reverse $ factorize' (abs n) 2 []
  where
    -- number -> divider -> acc -> factors
    factorize' :: Int -> Int -> [Int] -> [Int]
    factorize' 1 _ acc = acc
    factorize' n d acc =
      if n `mod` d == 0
      then factorize' (n `div` d) d (d : acc)
      else factorize' n (d + 1) acc

{-
  Задание 4
  Дан список целых чисел. Найти в нем наиболее часто повторяющееся число. В случае,
  если таких чисел несколько, вернуть максимальное из них.
-}

mostFrequent :: [Int] -> Int
mostFrequent lst = fst $ head $ sortBy cmp $ frequencies lst
  where  
    frequencies :: [Int] -> [(Int, Int)]
    frequencies lst = toList $ fromListWith (+) $ Prelude.map (\x -> (x, 1)) lst
    
    cmp :: (Int, Int) -> (Int, Int) -> Ordering
    cmp (k1, v1) (k2, v2) = case v2 `compare` v1 of
      EQ -> k2 `compare` k1
      neq -> neq

{-
  Задание 5
  Дана база данных о продажах некоторого интернет-магазина. Каждая строка входного
  файла представляет собой запись вида:
  Покупатель товар цена дата
  Покупатель — имя покупателя (строка без пробелов), товар — название товара (строка
  без пробелов), цена - стоимость товара (целое число), дата - дата покупки в 
  формате XX.XX.XXXX
  
  Для каждого месяца верните имя того, кто совершил самую дорогую покупку, и 
  название купленного товара.
  
  Пример:
  Sergey sneakers 13000 15.01.2018
  Kirill mobile 15000 16.01.2018
  Sergey headphones 10000 01.02.2018
  
  Вывод:
  01.2018:
  Kirill mobile
  02.2018:
  Sergey headphones
-}

-- (прототип функции можно изменить)
mostExpencive :: [String] -> [(String, String, String)]
mostExpencive _ = todo
