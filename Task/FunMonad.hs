module Task.FunMonad where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
  -- Композиция функций
  fmap f (FunMonad g) = FunMonad $ f . g

instance Applicative FunMonad where
  -- Тривиальная обертка -- функция которая всегда возвращает этот элемент
  pure x = FunMonad $ \_ -> x
  
  -- Создаем новую функцию, которая принимает аргумент,
  -- сначала передает его во вторую функции,
  -- затем аргумент вместе с результатом второй функции передает в первую
  -- f :: (String -> a) -> b, g :: (String -> a), res :: (String -> b)
  (FunMonad f) <*> (FunMonad g) = FunMonad $ \s -> f s (g s)

instance Monad FunMonad where
  -- Создаем новую функцию, которая принимает аргумент,
  -- сначала передает его в первую функцию,
  -- затем результат первой функции передает во вторую,
  -- после чего применяет к полученной функции аргумент
  -- f :: String -> a, g :: a -> String -> b, res :: String -> b
  (FunMonad f) >>= g = FunMonad $ \s -> fun (g $ f s) s
