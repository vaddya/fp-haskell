module Stepik.Data.Sum where

{-
  Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.
-}
data Color = Red | Green | Blue

instance Show Color where
  show Red    = "Red"
  show Green  = "Green"
  show Blue   = "Blue"

{-
  Определите частичную (определенную на значениях от '0' до '9') функцию charToInt.
-}
charToInt :: Char -> Int
charToInt x | x `elem` ['0'..'9'] = read [x]

{-
  Определите (частичную) функцию stringToColor, которая по строковому представлению 
  цвета как в прошлой задаче возвращает исходный цвет.
-}
stringToColor :: String -> Color
stringToColor "Red"   = Red
stringToColor "Green" = Green
stringToColor "Blue"  = Blue

{-
  Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.
-}
data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Info Info = EQ
cmp Error Error = EQ
cmp Warning Warning= EQ
cmp Error _ = GT
cmp _ Error = LT
cmp Info _ = LT
cmp _ Info = GT

{-
  Определите функцию processData, которая вызывает doSomeWork и возвращает 
  строку "Success" в случае ее успешного завершения, 
  либо строку "Fail: N" в случае неудачи, где N — код ошибки.
-}
data Result = Fail | Success
data SomeData

doSomeWork :: SomeData -> (Result, Int)
doSomeWork _ = (Success, 42)

processData :: SomeData -> String
processData x = 
  case doSomeWork x of
    (Success, _) -> "Success"
    (Fail, code) -> "Fail: " ++ show code
