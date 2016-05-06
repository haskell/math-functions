-- | Helpers for testing
module Tests.Helpers (
    -- * helpers
    T(..)
  , typeName
  , eq
  , eqC
    -- * Generic QC tests
  , monotonicallyIncreases
    -- * HUnit helpers
  , testAssertion
  , testEquality
  ) where

import Data.Complex
import Data.Typeable

import qualified Test.HUnit      as HU
import Test.Framework
import Test.Framework.Providers.HUnit




----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Phantom typed value used to select right instance in QC tests
data T a = T

-- | String representation of type name
typeName :: Typeable a => T a -> String
typeName = show . typeOf . typeParam
  where
    typeParam :: T a -> a
    typeParam _ = undefined

-- | Approximate equality for 'Double'. Doesn't work well for numbers
--   which are almost zero.
eq :: Double                    -- ^ Relative error
   -> Double -> Double -> Bool
eq eps a b 
  | a == 0 && b == 0 = True
  | otherwise        = abs (a - b) <= eps * max (abs a) (abs b)

-- | Approximate equality for 'Complex Double'
eqC :: Double                   -- ^ Relative error
    -> Complex Double
    -> Complex Double
    -> Bool
eqC eps a@(ar :+ ai) b@(br :+ bi)
  | a == 0 && b == 0 = True
  | otherwise        = abs (ar - br) <= eps * d
                    && abs (ai - bi) <= eps * d
  where
    d = max (realPart $ abs a) (realPart $ abs b)



----------------------------------------------------------------
-- Generic QC
----------------------------------------------------------------

-- Check that function is nondecreasing
monotonicallyIncreases :: (Ord a, Ord b) => (a -> b) -> a -> a -> Bool
monotonicallyIncreases f x1 x2 = f (min x1 x2) <= f (max x1 x2)



----------------------------------------------------------------
-- HUnit helpers
----------------------------------------------------------------

testAssertion :: String -> Bool -> Test
testAssertion str cont = testCase str $ HU.assertBool str cont

testEquality :: (Show a, Eq a) => String -> a -> a -> Test
testEquality msg a b = testCase msg $ HU.assertEqual msg a b
