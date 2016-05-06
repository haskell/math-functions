-- |
-- Tests for approximate comparison
module Tests.Comparison (tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Tests.Helpers

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
    --
  , testAssertion "relativeError inf   1" $ isNaN $ relativeError inf 1
  , testAssertion "relativeError 1   inf" $ isNaN $ relativeError 1 inf
  , testAssertion "relativeError -inf  1" $ isNaN $ relativeError (-inf) 1
  , testAssertion "relativeError 1  -inf" $ isNaN $ relativeError 1 (-inf)
  , testAssertion "relativeError inf inf" $ isNaN $ relativeError inf inf
  , testAssertion "relativeError inf-inf" $ isNaN $ relativeError inf (-inf)
  , testAssertion "relativeError   1 Nan" $ isNaN $ relativeError 1 nan
  , testAssertion "relativeError NaN   1" $ isNaN $ relativeError nan 1
  , testAssertion "relativeError NaN Nan" $ isNaN $ relativeError nan nan
  ]
  where
    inf = 1/0
    nan = 0/0
