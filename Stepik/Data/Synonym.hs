module Stepik.Data.Synonym where

import Prelude hiding (lookup)
import qualified Data.List as L

{-
  Реализуйте представителя класса типов Monoid для типа Xor, в котором mappend выполняет операцию xor.
-}
newtype Xor = Xor { getXor :: Bool }
  deriving (Eq, Show)

instance Semigroup Xor where
  a <> b = Xor $ a /= b

instance Monoid Xor where
  mempty = Xor False

{-
  Реализуйте представителя класса типов Monoid для Maybe' a так, чтобы mempty не был равен Maybe' Nothing. 
  Нельзя накладывать никаких дополнительных ограничений на тип a, кроме указанных в условии.
-}
newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
  deriving (Eq,  Show)

instance (Semigroup a) => Semigroup (Maybe' a) where
  (Maybe' Nothing) <> _ = Maybe' Nothing
  _ <> (Maybe' Nothing) = Maybe' Nothing
  (Maybe' a) <> (Maybe' b) = Maybe' $ a `mappend` b

instance Monoid a => Monoid (Maybe' a) where
  mempty = Maybe' $ Just mempty

{-
  Ниже приведено определение класса MapLike типов, похожих на тип Map. Определите представителя MapLike 
  для типа ListMap, определенного ниже как список пар ключ-значение. Для каждого ключа должно храниться 
  не больше одного значения. Функция insert заменяет старое значение новым, если ключ уже содержался в структуре.
-}
class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k,v)] -> m k v
  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
  deriving (Eq,Show)
    
instance MapLike (ListMap) where
  empty = ListMap []
  lookup _ (ListMap []) = Nothing
  lookup key (ListMap ((k, v) : xs))
    | k == key  = Just v
    | otherwise = lookup key $ ListMap xs
  insert key value (ListMap []) = ListMap [(key, value)]
  insert key value (ListMap (x@(k, v) : xs))
    | k == key  = ListMap ((key, value) : xs)
    | otherwise = ListMap (x : rest)
      where ListMap rest = insert key value $ ListMap xs
  delete key a@(ListMap []) = a
  delete key (ListMap (x@(k, v) : xs))
    | k == key  = ListMap (xs)
    | otherwise = ListMap (x : rest)
      where ListMap rest = delete key $ ListMap xs

{-
  Реализуйте представителя MapLike для типа ArrowMap, определенного ниже.
-}
newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
  empty = ArrowMap $ const Nothing
  lookup key (ArrowMap arrow) = arrow key
  insert key value (ArrowMap arrow) =
    ArrowMap $ \k -> if k == key then Just value else arrow k
  delete key (ArrowMap arrow) =
    ArrowMap $ \k -> if k == key then Nothing else arrow k
