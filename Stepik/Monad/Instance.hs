module Stepik.Monad.Instance where

{-
  Реализуйте вычисление с логированием, используя Log.
-}
data Log a = Log [String] a

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g =
  let
    (Log [msg1] val1) = f x
    (Log [msg2] val2) = g val1
  in Log [msg1, msg2] val2

{-
  Функции с логированием из предыдущего задания возвращают в качестве результата значение 
  с некоторой дополнительной информацией в виде списка сообщений. Этот список является контекстом. 
  Реализуйте функцию returnLog
-}
returnLog :: a -> Log a
returnLog = Log []

{-
  Реализуйте фукцию bindLog, которая работает подобно оператору >>= для контекста Log.
-}
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log m1 x1) k =
  let
    (Log m2 x2) = k x1
  in Log (m1 <> m2) x2

{-
  Реализованные ранее returnLog и bindLog позволяют объявить тип Log представителем класса Monad.
  Используя return и >>=, определите функцию execLoggersList.
-}
instance Functor Log where
  fmap f (Log log a) = Log log (f a)

instance Applicative Log where
  pure = returnLog
  (Log log1 f) <*> (Log log2 a) = Log (log1 <> log2) (f a)

instance Monad Log where
  return = returnLog
  (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return

{-
  Если некоторый тип является представителем класса Monad, то его можно сделать представителем класса Functor, 
  используя функцию return и оператор >>=. Причём, это можно сделать даже не зная, как данный тип устроен.
-}
data SomeType a

instance Applicative SomeType where

instance Monad SomeType where

instance Functor SomeType where
  fmap f x = x >>= (return . f)
