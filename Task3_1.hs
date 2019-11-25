module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

peanoToInt :: WeirdPeanoNumber -> Integer
peanoToInt n = case n of 
  Zero     -> 0
  (Succ p) -> peanoToInt p + 1
  (Pred p) -> peanoToInt p - 1

peanoFromInt :: Integer -> WeirdPeanoNumber
peanoFromInt n 
  | n == 0     = Zero
  | n > 0      = Succ $ peanoFromInt (n - 1)
  | otherwise  = Pred $ peanoFromInt (n + 1)

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize p = peanoFromInt $ peanoToInt p

instance Show WeirdPeanoNumber where
  show Zero     = "0"
  show (Succ p) = "+" ++ show p
  show (Pred p) = "-" ++ show p

instance Eq WeirdPeanoNumber where
  p1 == p2 = (normalize p1) === (normalize p2)
    where
      Zero      === Zero      = True
      (Succ p1) === (Succ p2) = p1 === p2
      (Pred p1) === (Pred p2) = p1 === p2
      _         === _         = False
        
instance Ord WeirdPeanoNumber where
  compare p1 p2 = compare' (normalize p1) (normalize p2)
    where
      compare' Zero Zero           = EQ
      compare' Zero (Succ p2)      = LT
      compare' Zero (Pred p2)      = GT
      compare' (Succ p1) (Succ p2) = compare' p1 p2
      compare' (Succ p1) _         = GT
      compare' (Pred p1) (Pred p2) = compare' p1 p2
      compare' (Pred p1) _         = LT

instance Enum WeirdPeanoNumber where
  toEnum   = peanoFromInt . fromIntegral
  fromEnum = fromIntegral . peanoToInt
  succ x   = Succ x
  pred x   = Pred x

instance Num WeirdPeanoNumber where
  p1 + Zero      = p1
  p1 + (Succ p2) = Succ $ p1 + p2
  p1 + (Pred p2) = Pred $ p1 + p2
  
  p1 * Zero   = Zero
  p1 * (Succ p2) = p1 * p2 + p1
  p1 * (Pred p2) = p1 * p2 - p1

  abs Zero = 0
  abs (Succ p) = abs p + 1
  abs (Pred p) = abs p + 1
  
  signum p = case normalize p of
    Zero     -> Zero
    (Succ _) -> Succ Zero
    (Pred _) -> Pred Zero

  fromInteger i
    | i == 0     = Zero
    | i > 0      = Succ $ fromInteger (i - 1)
    | otherwise  = Pred $ fromInteger (i + 1)

  negate Zero = Zero
  negate (Succ p) = Pred $ negate p
  negate (Pred p) = Succ $ negate p

instance Real WeirdPeanoNumber where
  toRational = toRational . toInteger

instance Integral WeirdPeanoNumber where
  toInteger p = peanoToInt p

  quotRem p1 p2 = quotRem' (abs p1) (abs p2) Zero
    where
      s1 = signum p1
      s2 = signum p2
      quotRem' x y acc
        | x < y     = (s1 * s2 * acc, s1 * x)
        | otherwise = quotRem' (x - y) y (Succ acc)
