{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Sum (tests) where

import Control.Applicative ((<$>))
import Numeric.Sum as Sum
import Numeric.MathFunctions.Comparison
import Prelude hiding (sum)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.QuickCheck (Arbitrary(..))
import qualified Prelude

-- Test that summation result is same as exact sum. That should pass
-- if we're effectively working with quad precision
t_sum :: ([Double] -> Double) -> [Double] -> Property
t_sum f xs
  = counterexample ("APPROX = " ++ show approx)
  $ counterexample ("EXACT  = " ++ show exact)
  $ counterexample ("DELTA  = " ++ show (approx - exact))
  $ counterexample ("ULPS   = " ++ show (ulpDistance approx exact))
  $ approx == exact
  where
    approx = f xs
    exact  = trueSum xs

-- Test that summation has smaller error than naive summation or no
-- worse than given number of ulps. If we're close enough to exact
-- answer naive may get ahead
t_sum_error :: ([Double] -> Double) -> [Double] -> Property
t_sum_error f xs
  = counterexample ("APPROX  = " ++ show approx)
  $ counterexample ("NAIVE   = " ++ show naive)
  $ counterexample ("EXACT   = " ++ show exact)
  $ counterexample ("A-EXACT = " ++ show (approx - exact))
  $ counterexample ("N-EXACT = " ++ show (naive  - exact))
  $ counterexample ("ULPS[A] = " ++ show (ulpDistance approx exact))
  $ counterexample ("ULPS[N] = " ++ show (ulpDistance naive  exact))
  $ abs (exact - approx) <= abs (exact - naive)
  where
    naive  = Prelude.sum xs
    approx = f xs
    exact  = trueSum xs

t_sum_shifted :: ([Double] -> Double) -> [Double] -> Property
t_sum_shifted f = t_sum_error f . zipWith (+) badvec

trueSum :: (Fractional b, Real a) => [a] -> b
trueSum xs = fromRational . Prelude.sum . map toRational $ xs

badvec :: [Double]
badvec = cycle [1, 1e14, -1e14]

tests :: TestTree
tests = testGroup "Summation"
  [ testGroup "Kahan" [
      -- Kahan summation only beats naive summation when truly
      -- catastrophic cancellation occurs
      testProperty "t_sum_shifted" $ t_sum_shifted (sum kahan)
    ]
  , testGroup "KBN" [
      testProperty "t_sum"         $ t_sum         (sum kbn)
    , testProperty "t_sum_error"   $ t_sum_error   (sum kbn)
    , testProperty "t_sum_shifted" $ t_sum_shifted (sum kbn)
    ]
  , testGroup "KB2" [
      testProperty "t_sum"         $ t_sum         (sum kb2)
    , testProperty "t_sum_error"   $ t_sum_error   (sum kb2)
    , testProperty "t_sum_shifted" $ t_sum_shifted (sum kb2)
    ]
  ]

instance Arbitrary KahanSum where
    arbitrary = toKahan <$> arbitrary
    shrink = map toKahan . shrink . fromKahan

toKahan :: (Double, Double) -> KahanSum
toKahan (a,b) = KahanSum a b

fromKahan :: KahanSum -> (Double, Double)
fromKahan (KahanSum a b) = (a,b)

instance Arbitrary KBNSum where
    arbitrary = toKBN <$> arbitrary
    shrink = map toKBN . shrink . fromKBN

toKBN :: (Double, Double) -> KBNSum
toKBN (a,b) = KBNSum a b

fromKBN :: KBNSum -> (Double, Double)
fromKBN (KBNSum a b) = (a,b)

instance Arbitrary KB2Sum where
    arbitrary = toKB2 <$> arbitrary
    shrink = map toKB2 . shrink . fromKB2

toKB2 :: (Double, Double, Double) -> KB2Sum
toKB2 (a,b,c) = KB2Sum a b c

fromKB2 :: KB2Sum -> (Double, Double, Double)
fromKB2 (KB2Sum a b c) = (a,b,c)
