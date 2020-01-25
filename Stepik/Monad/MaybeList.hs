module Stepik.Monad.MaybeList where

import Data.Char (isDigit, digitToInt)

{-
  Реализуйте лексер арифметических выражений.
-}
data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken str = case str of
  "+" -> Just Plus
  "-" -> Just Minus
  "(" -> Just LeftBrace
  ")" -> Just RightBrace
  s   -> toNumber s >>= Just . Number

toNumber :: String -> Maybe Int
toNumber [] = Nothing
toNumber str = helper str (Just 0)
  where
    helper :: String -> Maybe Int -> Maybe Int
    helper _ Nothing = Nothing
    helper [] sum = sum
    helper (x : xs) (Just sum) =
      if isDigit x
      then helper xs (Just (sum * 10 + digitToInt x))
      else Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . map asToken . words

{-
  Пусть имеется тип данных, который описывает конфигурацию шахматной доски.
  Напишите функцию, которая принимает конфигурацию доски, число ходов n, предикат и возвращает все 
  возможные конфигурации досок, которые могут получиться, если фигуры сделают n ходов и которые 
  удовлетворяют заданному предикату. При n < 0 функция возвращает пустой список.
-}
data Board = Board

nextPositions :: Board -> [Board]
nextPositions = undefined

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN current 0 pred = if (pred current) then [current] else []
nextPositionsN current n pred = do
  x <- if n < 0 then [] else nextPositions current
  nextPositionsN x (n - 1) pred

{-
  Используя монаду списка и do-нотацию, реализуйте функцию, которая принимает на вход некоторое число xx и возвращает список троек (a, b, c) таких что
  a^2 + b^2 = c^2, a > 0, b > 0, c > 0, c <= x, a < b
-}
pythagoreanTriple :: Int -> [(Int, Int, Int)]
--pythagoreanTriple x = [(a, b, c) | a <- [1..x], b <- [1..x], c <- [1..x], c <= x, a <= b, a * a + b * b == c * c]
pythagoreanTriple x = do
  c <- [1..x]
  b <- [1..c]
  a <- [1..b]
  True <- return $ a^2 + b^2 == c^2
  return (a, b, c)
