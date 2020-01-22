module Test.Math (reportMath) where

import Test
import Task.Math
import Prelude hiding (gcd, cos, sin)
import qualified Prelude (gcd, cos, sin)

-- Test sin, cos, gcd, pow by comparing absolute difference
cmp :: (Ord a, Num a) => (a -> a) -> (a -> a) -> a -> a -> Bool
cmp f g e x = abs (f x - g x) <= e

cmpSin = cmp sin Prelude.sin 1e-3
cmpSins = [cmpSin (-1), cmpSin 0, cmpSin 1, cmpSin 3, cmpSin 6]

cmpCos = cmp cos Prelude.cos 1e-3
cmpCoss = [cmpCos (-1), cmpCos 0, cmpCos 1, cmpCos 3, cmpSin 6]

cmpGcd x y = gcd x y == Prelude.gcd x y
cmpGcds = [cmpGcd 15 5, cmpGcd 42 56, cmpGcd (-42) 56, cmpGcd 0 1, cmpGcd 1701 3768]

cmpPow x n = pow x n == x ^ n
cmpPows = [cmpPow 3 5, cmpPow 5 3, cmpPow 0 1, cmpPow 1 0, cmpPow 2 100]

reportMath = reportAll [cmpSins, cmpCoss, cmpGcds, cmpPows]