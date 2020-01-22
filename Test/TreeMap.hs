module Test.TreeMap (reportTreeMap) where

import Test
import Task.TreeMap
import Prelude hiding (lookup)
import qualified Prelude (lookup)

-- Test
tree = emptyTree |++| (2, "2") |++| (1, "1") |++| (5, "5") |--| 2 |++| (3, "3") |++| (0, "0") |++| (6, "6") |++| (-2, "-2")
--       1
--    0      5
-- -2      3   6

testTreeSize = [treeSize tree == 6]

testContain key expected = actual == expected
  where actual = contains key tree
testContains = [testContain 2 False, testContain 1 True, testContain 4 False]

testLookup key expected = actual == expected
  where actual = lookup key tree
testLookups = [testLookup 2 Nothing, testLookup 1 $ Just "1", testLookup 5 $ Just "5"]

testNearestLE key expected = actual == expected
  where actual = nearestLE key tree
testNearestLEs = [testNearestLE 2 $ Just (1, "1"), testNearestLE 7 $ Just (6, "6"), testNearestLE 3 $ Just (3, "3")]

testKMean k expected = actual == expected
  where actual = fst $ kMean k tree
testKMeans = [testKMean 0 (-2), testKMean 3 3]

reportTreeMap = reportAll [testTreeSize, testContains, testLookups, testNearestLEs, testKMeans]