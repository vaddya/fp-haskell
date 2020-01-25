module Stepik.Data.Record where

import Data.Time.Clock
import Data.Time.Format

{-
  Определите тип записи, который хранит элементы лога:
    timestamp — время, когда произошло событие (типа UTCTime);
    logLevel — уровень события (типа LogLevel);
    message — сообщение об ошибке (типа String).
  Определите функцию logLevelToString, возвращающую текстуальное представление 
  типа LogLevel, и функцию logEntryToString, возвращающую текстуальное представление записи в виде:
  <время>: <уровень>: <сообщение>
-}
timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString x = case x of
  Error   -> "Error"
  Warning -> "Warning"
  Info    -> "Info"

logEntryToString :: LogEntry -> String
logEntryToString x = timeToString (x & timestamp) ++ ": "
  ++ logLevelToString (x & logLevel) ++ ": " ++ (x & message)

(&) x f = f x

{-
  Определите функцию updateLastName person1 person2, которая меняет фамилию person2 на фамилию person1
-}
data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person2 { lastName = lastName person1 }

{-
  Определить функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть, 
  если имя было "Ivan", то после применения этой функции оно превратится в "I.". 
  Однако, если имя было короче двух символов, то оно не меняется.
-}
abbrFirstName :: Person -> Person
abbrFirstName p =
  case firstName p of
    (x : _ : _) -> p { firstName = x : "." }
    _           -> p