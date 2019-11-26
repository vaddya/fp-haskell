module Task4_1 where

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
  fmap f (FunMonad g) = FunMonad $ f . g

instance Applicative FunMonad where
  pure x = FunMonad $ \_ -> x
  (FunMonad f) <*> (FunMonad g) = FunMonad $ \s -> f s $ g s

instance Monad FunMonad where
  (FunMonad f) >>= g = FunMonad $ \s -> fun (g $ f s) s
