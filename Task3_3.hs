module Task3_3 where

{-
  Задание 3.3
  Множество на основе предикатов
-}

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- Комбинирование двух множеств можно реализовать по разному
-- в зависимости от выбранной операции: AND, OR, XOR

-- Union (AND)
newtype PSetUnion a = PSetUnion{ unionContains :: (a -> Bool) }

instance Semigroup (PSetUnion a) where
  s1 <> s2 = PSetUnion $ \x -> unionContains s1 x || unionContains s2 x 

instance Monoid (PSetUnion a) where
  mempty = PSetUnion $ \_ -> False
  
-- Intersection (OR)
newtype PSetIntersect a = PSetIntersect{ intersectionContains :: (a -> Bool) }

instance Semigroup (PSetIntersect a) where
  s1 <> s2 = PSetIntersect $ \x -> intersectionContains s1 x && intersectionContains s2 x 

instance Monoid (PSetIntersect a) where
  mempty = PSetIntersect $ \_ -> True
  
-- Symmetric difference (XOR)
xor :: Bool -> Bool -> Bool
xor a b = not $ a == b

newtype PSetSymDiff a = PSetSymDiff{ symDiffContains :: (a -> Bool) }

instance Semigroup (PSetSymDiff a) where
  s1 <> s2 = PSetSymDiff $ \x -> symDiffContains s1 x `xor` symDiffContains s2 x 

instance Monoid (PSetSymDiff a) where
  mempty = PSetSymDiff $ \_ -> False

-- Ничего не знаем о множестве, формируемым операцией fmap
-- поэтому результат всегда False
instance Functor PSet where
  fmap _ _ = PSet (\_ -> False)
