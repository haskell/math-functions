-- |
-- Tests for approximate comparison
module Tests.Comparison (tests) where

import Test.QuickCheck  hiding (choose)
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Tests.Helpers
import Tests.SpecFunctions.Tables

import Numeric.MathFunctions.Comparison
import Numeric.MathFunctions.Constants (m_epsilon)

tests :: Test
tests = testGroup "Comparison"
  [ testProperty "addUlps 0"       $ \x   -> x == addUlps 0 x
  , testProperty "addUlps sym"     $ \i x -> x == (addUlps (-i) . addUlps i) x
  , testProperty "ulpDistance==0"  $ \x   -> ulpDistance x x == 0
  , testProperty "ulpDistance sym" $ \x y -> ulpDistance x y == ulpDistance y x
  , testProperty "ulpDistance/addUlps" $ \x i -> ulpDistance x (addUlps i x) == fromIntegral (abs i)
    -- Test that code is correct for m_epsilon
  , testAssertion "eps distance" $ ulpDistance 1 (1+m_epsilon) == 1
  , testAssertion "eps add"      $ addUlps 1 1 == 1 + m_epsilon
  ]
