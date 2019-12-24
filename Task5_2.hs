module Task5_2 where

import Todo(todo)
import Prelude hiding (concat)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

toList :: Zipper a -> [a]
toList z@(Zipper l r)
  | null l    = r
  | otherwise = toList $ goLeft z

instance (Show a) => Show (Zipper a) where
  show = show . toList

instance (Eq a) => Eq (Zipper a) where
  z1 == z2 = toList z1 == toList z2

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

concat :: Zipper a -> Zipper a -> Zipper a
concat (Zipper l []) (Zipper [] r) = Zipper l r
concat left right = concat (goRight left) (goLeft right)

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index what into = todo

subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to input = todo
