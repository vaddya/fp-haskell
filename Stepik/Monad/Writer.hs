module Stepik.Monad.Writer where

import Data.Monoid (Sum(Sum), getSum)

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where

instance Applicative (Writer w) where

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= k =
    let (x, u) = runWriter m
        (y, v) = runWriter $ k x
    in Writer (y, u `mappend` v)

{-
  Функция execWriter запускает вычисление, содержащееся в монаде Writer, и возвращает получившийся лог, 
  игнорируя сам результат вычисления. Реализуйте функцию evalWriter, которая, наоборот, игнорирует 
  накопленный лог и возвращает только результат вычисления.
-}
execWriter :: Writer w a -> w
execWriter m = snd $ runWriter m

evalWriter :: Writer w a -> a
evalWriter m = fst $ runWriter m

{-
  Давайте разработаем программное обеспечение для кассовых аппаратов одного исландского магазина. 
  Заказчик собирается описывать товары, купленные покупателем, с помощью типа Shopping.
  Последовательность приобретенных товаров записывается с помощью do-нотации. 
  Для этого используется функция purchase, которую вам предстоит реализовать. 
  Эта функция принимает наименование товара, а также его стоимость в исландских кронах 
  (исландскую крону не принято делить на меньшие единицы, потому используется целочисленный тип Integer). 
  Кроме того, вы должны реализовать функцию total.
-}
tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

type Shopping = Writer (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

purchase :: String -> Integer -> Shopping
purchase _ = tell . Sum

total :: Shopping -> Integer
total = getSum . execWriter

{-
  Измените определение типа Shopping и доработайте функцию purchase из предыдущего задания таким образом, 
  чтобы можно было реализовать функцию items, возвращающую список купленных товаров.
-}
type Shopping' = Writer (Sum Integer, [String]) ()

purchase' :: String -> Integer -> Shopping'
purchase' item cost = tell (Sum cost, [item])

total' :: Shopping' -> Integer
total' = getSum . fst . execWriter  

items' :: Shopping' -> [String]
items' = snd. execWriter
