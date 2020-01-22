module Test.Term (reportTerm) where

import Test
import Task.Term

-- Test replace variable
testReplace :: Int -> String -> Term -> Term -> Bool
testReplace expected varName replacement expression = actual == expected
  where actual = (intValue . evaluate) $ replaceVar varName replacement expression

testReplace1 = testReplace 8 "test" (IntConstant 4) (IntConstant 2 |*| Variable "test")
testReplace2 = testReplace 2 "test" (IntConstant 0) (IntConstant 2 |+| Variable "test" |*| IntConstant 100)
testReplace3 = testReplace 5 "test" (IntConstant 5) (IntConstant 5)

-- Test evaluate term
testEval :: Int -> Term -> Bool
testEval expected term = actual == expected
  where actual = (intValue . evaluate) term

testEval1 = testEval 8 (IntConstant 4 |*| (IntConstant 4 |-| IntConstant 1 |*| IntConstant 2))
testEval2 = testEval 6 (IntConstant 1 |+| IntConstant 3 |*| IntConstant 5 |-| IntConstant 10)
testEval3 = testEval 5 (IntConstant 5)

reportTerm = report [testReplace1, testReplace2, testReplace3, testEval1, testEval2, testEval3]
