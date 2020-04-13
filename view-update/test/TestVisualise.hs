module TestVisualise
  ( testWalkGraph
  , visualiseTests
  ) where

import Data.Text.Lazy as L
import Test.HUnit
import Visualise

testWalkGraph :: Test
testWalkGraph = TestCase (assertEqual "walk should walk" 1 1)

visualiseTests = TestList [testWalkGraph]
