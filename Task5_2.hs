module Task5_2 where

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
--   show (Zipper l r) = show l ++ show r -- debug

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

-- concat (Zipper [] [1,2,3]) (Zipper [] [4,5,6]) -> Zipper [] [1,2,3,4,5,6]
concat :: Zipper a -> Zipper a -> Zipper a
concat left@(Zipper ll lr) right =
  let Zipper _ rr = reset right 
  in Zipper ll (lr ++ rr)

-- reset $ insertManyAt 2 (Zipper [] [5,6]) (Zipper [] [1,2,3,4]) -> Zipper [] [1,2,5,6,3,4]
insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index what@(Zipper wl wr) into = 
  let Zipper il ir = goRightN index $ reset into
  in Zipper (wl ++ il) (wr ++ ir)

-- reset $ subZipper 2 4 (Zipper [] [1,2,3,4,5]) -> Zipper [] [3,4]
subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to input
  | from > to  = error("from > to")
  | otherwise  = reset $ dropRight $ goRightN (to - from) $ dropLeft $ goRightN from $ reset input

-- Utils
reset :: Zipper a -> Zipper a
reset z@(Zipper [] _) = z
reset z = reset $ goLeft z

goRightN :: Int -> Zipper a -> Zipper a
goRightN 0 z = z
goRightN n z = goRightN (n - 1) $ goRight z

dropLeft :: Zipper a -> Zipper a
dropLeft (Zipper _ r) = Zipper [] r

dropRight :: Zipper a -> Zipper a
dropRight (Zipper l _) = Zipper l []
