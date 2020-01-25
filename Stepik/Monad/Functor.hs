module Stepik.Monad.Functor where

{-
  Определите представителя класса Functor для следующего типа данных, представляющего точку в трёхмерном пространстве.
-}
data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
  fmap f (Point3D a b c) = Point3D (f a) (f b) (f c)

{-
  Определите представителя класса Functor для типа данных GeomPrimitive.
-}
data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
  fmap f (Point p) = Point $ fmap f p
  fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)

{-
  Определите представителя класса Functor для бинарного дерева, в каждом узле которого хранятся элементы типа Maybe.
-}
data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
  fmap f (Leaf Nothing) = Leaf Nothing
  fmap f (Leaf (Just x)) = Leaf $ Just $ f x
  fmap f (Branch l Nothing r) = Branch (fmap f l) Nothing (fmap f r)
  fmap f (Branch l (Just x) r) = Branch (fmap f l) (Just $ f x) (fmap f r)

{-
  Определите представителя класса Functor для типов данных Entry и Map.
-}
data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
  fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)

instance Functor (Map k1 k2) where
  fmap f (Map []) = Map []
  fmap f (Map ((Entry k v) : xs)) = Map ((Entry k (f v)) : rest)
    where (Map rest) = fmap f (Map xs)
