module Test.Fold where

import Test
import Task.Fold
import Prelude hiding (foldl, foldr, map, concatMap, filter, reverse, sum, product, elem)
import qualified Prelude (foldl, foldr, map, concatMap, filter, reverse, sum, product, elem)

testSum lst exptected = exptected == sum lst
testSums = [testSum [1,2,3] 6, testSum [] 0, testSum [-1,-2,-3,-4] (-10)]

testProduct lst expected = expected == product lst
testProducts = [testProduct [1,2,3] 6, testProduct [] 1, testProduct [0,1,2] 0]

testFilterPositive lst expected = expected == filterNot (\x -> x <= 0) lst
testFilterPositives = [testFilterPositive [-1,2,3] [2,3], testFilterPositive [-2,-3] [], testFilterPositive [1,2] [1,2]]

testReverse lst = (reverse . reverse) lst == lst
testReverses = [testReverse [1,2,3,4], testReverse [1]]

testRange f t s expected = expected == rangeTo f t s
testRanges = [testRange 1 10 3 [1,4,7], testRange (-5) 6 5 [-5,0,5], testRange 10 1 1 [], testRange 10 7 (-2) [10,8]]

testGroup lst n exptected = exptected == groups lst n
testGroups = [testGroup [1,2,3,4] 3 [[1,2,3],[4]], testGroup [1,2,3] 4 [[1,2,3]], testGroup [1,2,3] 1 [[1],[2],[3]]]

reportFold = reportAll [testSums, testProducts, testFilterPositives, testReverses, testRanges, testGroups]
