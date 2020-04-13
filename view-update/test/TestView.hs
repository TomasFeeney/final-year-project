module TestView
  ( viewTests
  ) where

import Test.HUnit
import qualified View as V

testSimpleFunc :: Test
testSimpleFunc =
  TestCase
    (assertEqual
       "One input and one output returns its own ID"
       (simpleFunc 2)
       (V.label 1 4))
  where
    simpleFunc :: Integer -> (V.Node Integer)
    simpleFunc _x = x * 2
      where
        x = V.label 1 _x

testListFunc :: Test
testListFunc =
  TestCase
    (assertEqual
       "View datatype handles lists and functions on them correctly"
       (listFunc [2, 3, 4])
       (V.labelMultiple [1, 2, 3] 9))
  where
    listFunc :: [Integer] -> (V.Node Integer)
    listFunc _xs = sum xs
      where
        xs = V.assign _xs

testListFractional :: Test
testListFractional =
  TestCase
    (assertEqual
       "View data type handles list in and list outputs correctly"
       (listFractional [1 / 4, 3 / 4, 4 / 4])
       ([(V.labelMultiple [1, 2] 4 / 4), (V.labelMultiple [1, 3] 3 / 4)]))
  where
    listFractional :: Fractional a => [a] -> [V.Node a]
    listFractional _xs = [x + y, z - x]
      where
        (x:y:z:_) = V.assign _xs

viewTests = [testSimpleFunc, testListFunc, testListFractional]
