module Task.TreeMap where

{-
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v =
    EmptyTree
  | Node Integer v (TreeMap v) (TreeMap v)
  deriving Show

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: Integer -> TreeMap v -> Bool
contains _ EmptyTree = False
contains x (Node k v t1 t2)
  | x == k    = True
  | x < k     = contains x t1
  | otherwise = contains x t2

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> Maybe v
lookup _ EmptyTree = Nothing
lookup x (Node k v t1 t2)
  | x == k    = Just v
  | x < k     = lookup x t1
  | otherwise = lookup x t2

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (x, y) EmptyTree = Node x y EmptyTree EmptyTree
insert xy@(x, y) (Node k v t1 t2)
  | x == k    = Node x y t1 t2
  | x < k     = Node k v (insert xy t1) t2
  | otherwise = Node k v t1 (insert xy t2)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ EmptyTree = EmptyTree
remove x (Node k v t1 t2)
  | x == k    = merge t1 t2
  | x < k     = Node k v (remove x t1) t2
  | otherwise = Node k v t1 (remove x t2)

merge :: TreeMap v -> TreeMap v -> TreeMap v
merge EmptyTree t2 = t2
merge t1 EmptyTree = t1
merge (Node k v t11 t12) t2 = Node k v t11 (merge t12 t2)

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> Maybe (Integer, v)
nearestLE i EmptyTree = Nothing
nearestLE i (Node k v t1 t2)
  | i == k = Just (k, v)
  | i < k  = nearestLE i t1
  | i > k  = case nearestLE i t2 of
     res@(Just (_, _)) -> res
     Nothing -> Just (k, v)

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList kvs = foldr insert EmptyTree kvs

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Node k v t1 t2) = concat [listFromTree t1, [(k, v)], listFromTree t2]

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i EmptyTree = error "IllegalArgumentException"
kMean i (Node k v t1 t2)
  | leftSize == i = (k, v)
  | leftSize > i  = kMean i t1
  | otherwise     = kMean (i - leftSize - 1) t2
  where
    leftSize = treeSize t1

treeSize :: TreeMap v -> Integer
treeSize EmptyTree = 0
treeSize (Node _ _ t1 t2) = 1 + treeSize t1 + treeSize t2

(|++|) :: TreeMap v -> (Integer, v) -> TreeMap v
(|++|) = flip insert

(|--|) :: TreeMap v -> Integer -> TreeMap v
(|--|) = flip remove
