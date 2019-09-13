{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Sum (tests) where

import Control.Applicative ((<$>))
import Numeric.Sum as Sum
import Prelude hiding (sum)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Prelude

t_sum :: ([Double] -> Double) -> [Double] -> Bool
t_sum f xs = f xs == trueSum xs

t_sum_error :: ([Double] -> Double) -> [Double] -> Bool
t_sum_error f xs = abs (ts - f xs) <= abs (ts - Prelude.sum xs)
  where ts = trueSum xs

t_sum_shifted :: ([Double] -> Double) -> [Double] -> Bool
t_sum_shifted f = t_sum_error f . zipWith (+) badvec

trueSum :: (Fractional b, Real a) => [a] -> b
trueSum xs = fromRational . Prelude.sum . map toRational $ xs

badvec :: [Double]
badvec = cycle [1,1e16,-1e16]

tests :: TestTree
tests = testGroup "Summation" [
    testGroup "ID" [
      -- plain summation loses precision quickly
      -- testProperty "t_sum" $ t_sum (sum id)

      -- tautological tests:
      -- testProperty "t_sum_error" $ t_sum_error (sum id)
      -- testProperty "t_sum_shifted" $ t_sum_shifted (sum id)
    ]
  , testGroup "Kahan" [
      -- tests that cannot pass:
      -- testProprty "t_sum" $ t_sum (sum kahan)
      -- testProperty "t_sum_error" $ t_sum_error (sum kahan)

      -- kahan summation only beats normal summation with large values
      testProperty "t_sum_shifted" $ t_sum_shifted (sum kahan)
    ]
  , testGroup "KBN" [
      testProperty "t_sum" $ t_sum (sum kbn)
    , testProperty "t_sum_error" $ t_sum_error (sum kbn)
    , testProperty "t_sum_shifted" $ t_sum_shifted (sum kbn)
    ]
  , testGroup "KB2" [
      testProperty "t_sum" $ t_sum (sum kb2)
    , testProperty "t_sum_error" $ t_sum_error (sum kb2)
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
