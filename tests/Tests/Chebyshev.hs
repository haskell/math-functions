{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Tests.Chebyshev ( tests ) where

import Data.Vector.Unboxed                  (fromList)
import Test.Tasty
import Test.Tasty.QuickCheck                (testProperty)
import Test.QuickCheck                      (Arbitrary(..),counterexample,Property)

import Tests.Helpers
import Numeric.Polynomial.Chebyshev
import Numeric.MathFunctions.Comparison


tests :: TestTree
tests = testGroup "Chebyshev polynomials"
  [ testProperty "Chebyshev 0" $ \a0 (Ch x) ->
      testCheb [a0] x
  , testProperty "Chebyshev 1" $ \a0 a1 (Ch x) ->
      testCheb [a0,a1] x
  , testProperty "Chebyshev 2" $ \a0 a1 a2 (Ch x) ->
      testCheb [a0,a1,a2] x
  , testProperty "Chebyshev 3" $ \a0 a1 a2 a3 (Ch x) ->
      testCheb [a0,a1,a2,a3] x
  , testProperty "Chebyshev 4" $ \a0 a1 a2 a3 a4 (Ch x) ->
      testCheb [a0,a1,a2,a3,a4] x
  , testProperty "Broucke" $ testBroucke
  ]
  where

testBroucke :: Ch -> [Double] -> Property
testBroucke _      []     = counterexample "" True
testBroucke (Ch x) (c:cs)
  = counterexample (">>> Chebyshev  = " ++ show c1)
  $ counterexample (">>> Brouke     = " ++ show cb)
  $ counterexample (">>> rel.err.   = " ++ show (relativeError c1 cb))
  $ counterexample (">>> diff. ulps = " ++ show (ulpDistance   c1 cb))
  $ within 64 c1 cb
  where
    c1 = chebyshev        x (fromList $ c : cs)
    cb = chebyshevBroucke x (fromList $ c*2 : cs)


testCheb :: [Double] -> Double -> Property
testCheb as x
  = counterexample (">>> Exact      = " ++ show exact)
  $ counterexample (">>> Numeric    = " ++ show num  )
  $ counterexample (">>> rel.err.   = " ++ show err  )
  $ counterexample (">>> diff. ulps = " ++ show (ulpDistance num exact))
  $ eq 1e-12 num exact
  where
    exact = evalCheb as x
    num   = chebyshev x (fromList as)
    err   = relativeError num exact

evalCheb :: [Double] -> Double -> Double
evalCheb as x
  = realToFrac
  $ sum
  $ zipWith (*) (map realToFrac as)
  $ map ($ realToFrac x) cheb

-- Chebyshev polynomials of low order
cheb :: [Rational -> Rational]
cheb =
  [ \_ -> 1
  , \x -> x
  , \x -> 2*x^2 - 1
  , \x -> 4*x^3 - 3*x
  , \x -> 8*x^4 - 8*x^2 + 1
  ]

-- Double in the [-1 .. 1] range
newtype Ch = Ch Double
             deriving Show
instance Arbitrary Ch  where
  arbitrary = do x <- arbitrary
                 return $ Ch $ 2 * (abs . snd . properFraction) x - 1
