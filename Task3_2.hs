module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil         = []
rlistToList (RCons xs x) = rlistToList xs ++ [x]

listToRList :: [a] -> ReverseList a
listToRList []  = RNil
listToRList lst = listToRList' $ reverse lst
  where 
    listToRList' []       = RNil
    listToRList' (x : xs) = RCons (listToRList' xs) x

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor
instance (Eq a) => Eq (ReverseList a) where
  RNil       == RNil       = True
  RNil       == _          = False
  _          == RNil       = False
  RCons xs x == RCons ys y = x == y && xs == ys 

instance (Ord a) => Ord (ReverseList a) where
  RNil       <= _          = True
  _          <= RNil       = False
  RCons xs x <= RCons ys y = x <= y && xs <= ys

instance (Show a) => Show (ReverseList a) where
  show RNil         = "[]"
  show (RCons xs x) = "[" ++ show' xs ++ show x ++ "]"
    where
      show' :: (Show a) => ReverseList a -> String
      show' RNil = ""
      show' (RCons xs x) = show' xs ++ show x ++ ","

instance Foldable ReverseList where
  foldMap f RNil         = mempty
  foldMap f (RCons xs x) = (foldMap f xs) <> (f x)

  foldr f z RNil         = z
  foldr f z (RCons xs x) = foldr f (f x z) xs

instance Semigroup (ReverseList a) where
  xs <> ys = foldl RCons xs ys

instance Monoid (ReverseList a) where
  mempty = RNil

instance Functor ReverseList where
  fmap f RNil         = RNil
  fmap f (RCons xs x) = RCons (fmap f xs) (f x)
