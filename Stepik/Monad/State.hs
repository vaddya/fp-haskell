module Stepik.Monad.State where

import Todo (todo)
import Stepik.Monad.Reader (Reader(Reader), runReader)
import Stepik.Monad.Writer (Writer(Writer), runWriter)
import Control.Monad (replicateM)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where

instance Applicative (State s) where

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= k = State $ \s ->
    let (a, s') = runState m s
        m' = k a
    in runState m' s'

evalState :: State s a -> s -> a
evalState m s = fst $ runState m s

execState :: State s a -> s -> s
execState m s = snd $ runState m s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

{-
  Напишите функцию readerToState, «поднимающую» вычисление из монады Reader в монаду State.
-}
readerToState :: Reader r a -> State r a
readerToState m = State $ \r -> (runReader m r, r)

{-
  Напишите функцию writerToState, «поднимающую» вычисление из монады Writer в монаду State.
-}
writerToState :: Monoid w => Writer w a -> State w a
writerToState m = State $ \r -> (a, r `mappend` w)
  where (a, w) = runWriter m

{-
  Вычисление n-ого числа Фиббоначи
-}
fibStep :: State (Integer, Integer) ()
fibStep = do
  (x, y) <- get
  put (y, x + y)

execStateN :: Int -> State s a -> s -> s
execStateN n m ini = execState (replicateM n m) ini

{-
  Требуется пронумеровать вершины дерева данной формы, обойдя их in-order 
  (то есть, сначала обходим левое поддерево, затем текущую вершину, затем правое поддерево).
-}
data Tree a = Leaf a | Fork (Tree a) a (Tree a)

numberTree :: Tree () -> Tree Integer
numberTree tree = todo
