-- |
module Tests.RootFinding ( tests ) where

import Data.Default.Class
import Test.Tasty
import Test.Tasty.HUnit

import Numeric.RootFinding
import Tests.Helpers


tests :: TestTree
tests = testGroup "Root finding"
  [ testGroup "Ridders"
    [ testAssertion "sin x - 0.525 [exact]"     $ testRiddersSin0_525 (AbsTol 0)
    , testAssertion "sin x - 0.525 [abs 1e-12]" $ testRiddersSin0_525 (AbsTol 1e-12)
    , testAssertion "sin x - 0.525 [abs 1e-6]"  $ testRiddersSin0_525 (AbsTol 1e-6)
    , testAssertion "sin x - 0.525 [rel 1e-12]" $ testRiddersSin0_525 (RelTol 1e-12)
    , testAssertion "sin x - 0.525 [rel 1e-6]"  $ testRiddersSin0_525 (RelTol 1e-6)
    ]
  , testGroup "Newton-Raphson"
    [ testAssertion "sin x - 0.525 [rel 1e-12]" $ testNewtonSin0_525 (RelTol 1e-12)
    , testAssertion "sin x - 0.525 [rel 1e-6]"  $ testNewtonSin0_525 (RelTol 1e-6)
    , testAssertion "sin x - 0.525 [abs 1e-12]" $ testNewtonSin0_525 (AbsTol 1e-12)
    , testAssertion "sin x - 0.525 [abs 1e-6]"  $ testNewtonSin0_525 (AbsTol 1e-6)
    , testAssertion "1/x - 0.5     [0]"         $
        let Root r = newtonRaphson def{newtonTol=RelTol 0} (1,1000,1000)
                       (\x -> (1/x - 0.5, -1/(x*x)))
        in  r == 2
    ]
  ]
  where
    -- Exact root for equation: sin x - 0.525 = 0
    exactRoot = 0.5527151130967832
    --
    testRiddersSin0_525 tol
      = withinTolerance tol r exactRoot
      where
        Root r = ridders def{riddersTol = tol} (0, pi/2) (\x -> sin x - 0.525)
    --
    testNewtonSin0_525 tol
      = withinTolerance tol r exactRoot
      where
        Root r = newtonRaphson def{newtonTol=tol} (0, pi/4, pi/2) (\x -> (sin x - 0.525, cos x))
