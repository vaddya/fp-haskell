module Stepik.Monad.Reader where

data Reader r a = Reader { runReader :: (r -> a) }

instance Functor (Reader r) where

instance Applicative (Reader r) where

instance Monad (Reader r) where
  return x = Reader $ \_ -> x
  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

{-
  Реализуйте функцию local'.
-}
local' :: (r -> r') -> Reader r' a -> Reader r a
local' f (Reader r) = Reader $ r . f

{-
  Реализуйте функцию, принимающую в качестве окружения UsersTable и 
  возвращающую список пользователей, использующих пароль "123456".
-}
type User = String
type Password = String
type UsersTable = [(User, Password)]

asks :: (r -> a) -> Reader r a
asks = Reader

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks $ \users -> [user | (user, "123456") <- users]
