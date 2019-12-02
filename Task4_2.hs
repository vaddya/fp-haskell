module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf { first :: a, second :: a, third :: a, fourth :: a } deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
  fmap f (FourOf x1 x2 x3 x4) = FourOf (f x1) (f x2) (f x3) (f x4)

instance Applicative FourOf where
  -- Тривиальная обертка -- повторение элемента 4 раза
  pure x = FourOf x x x x
  
  -- Применяем каждый элемент второй четверки к функциям из первой четверки
  -- f :: (a -> b) x 4, x :: (a) x 4, res :: (b) x 4
  (FourOf f1 f2 f3 f4) <*> (FourOf x1 x2 x3 x4) = FourOf (f1 x1) (f2 x2) (f3 x3) (f4 x4)

instance Monad FourOf where
  -- Применяем каждый элемент из четверки к функции, возвращающей четверку, и берем из нее только
  -- соответствующий элемент (для первого элемента берем первый элемент, для второго второй и т.д.)
  -- x :: (a) x 4, k :: a -> (b) x 4, res :: (b) x 4
  (FourOf x1 x2 x3 x4) >>= k = FourOf (first $ k x1) (second $ k x2) (third $ k x3) (fourth $ k x4)
