-- |
module Tests.RootFinding ( tests ) where

import Test.Framework
import Test.Framework.Providers.HUnit

import Numeric.RootFinding
import Tests.Helpers


tests :: Test
tests = testGroup "Root finding"
  [ testGroup "Ridders"
    [ testAssertion "sin x - 0.525 [exact]"  $ testRiddersSin0_525 0
    , testAssertion "sin x - 0.525 [1e-12]"  $ testRiddersSin0_525 1e-12
    , testAssertion "sin x - 0.525 [1e-6]"   $ testRiddersSin0_525 1e-6
    ]
  , testGroup "Newton-Raphson"
    [ testAssertion "sin x - 0.525 [1e-12]"  $ testNewtonSin0_525 1e-12
    , testAssertion "sin x - 0.525 [1e-6]"   $ testNewtonSin0_525 1e-6
    ]
  ]
  where
    -- Exact root for equation: sin x - 0.525 = 0
    exactRoot = 0.5527151130967832
    testRiddersSin0_525 tol
      = abs (r - exactRoot) <= tol
      where
        Root r = ridders tol (0, pi/2) (\x -> sin x - 0.525)
    testNewtonSin0_525 tol
      = abs (r - exactRoot) <= tol * r
      where
        Root r = newtonRaphson tol (0, pi/4, pi/2) (\x -> (sin x - 0.525, cos x))
