-- |
-- Module    : Numeric.MathFunctions.Comparison
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Approximate floating point comparison, based on Bruce Dawson's
-- \"Comparing floating point numbers\":
-- <http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm>
module Numeric.MathFunctions.Comparison
    (
      addUlps
    , ulpDistance
    , within
    ) where

import Control.Monad.ST (runST)
import Data.Primitive.ByteArray (newByteArray, readByteArray, writeByteArray)
import Data.Word (Word64)
import Data.Int (Int64)



-- |
-- Add N ULPs (units of least precision) to @Double@ number.
addUlps :: Int -> Double -> Double
addUlps n a = runST $ do
  buf <- newByteArray 8
  ai0 <- writeByteArray buf 0 a >> readByteArray buf 0
  -- Convert to ulps number represented as Int64
  let big     = 0x8000000000000000
      order :: Word64 -> Int64
      order i | i < big   = fromIntegral i
              | otherwise = fromIntegral $ maxBound - (i - big)
      unorder :: Int64 -> Word64
      unorder i | i >= 0    = fromIntegral i
                | otherwise = big + (maxBound - (fromIntegral i))
  let ai0' = unorder $ order ai0 + fromIntegral n
  writeByteArray buf 0 ai0' >> readByteArray buf 0

-- |
-- Measure distance between two @Double@s in ULPs (units of least
-- precision).
ulpDistance :: Double
            -> Double
            -> Word64
ulpDistance a b = runST $ do
  buf <- newByteArray 8
  ai0 <- writeByteArray buf 0 a >> readByteArray buf 0
  bi0 <- writeByteArray buf 0 b >> readByteArray buf 0
  -- IEEE754 floats use most significant bit as sign bit (not
  -- 2-complement) and we need to rearrange representations of float
  -- number so that they could be compared lexicographically as
  -- Word64.
  let big     = 0x8000000000000000
      order i | i < big   = i + big
              | otherwise = maxBound - i
      ai = order ai0
      bi = order bi0
      d  | ai > bi   = ai - bi
         | otherwise = bi - ai
  return $! d

-- | Compare two 'Double' values for approximate equality, using
-- Dawson's method.
--
-- The required accuracy is specified in ULPs (units of least
-- precision).  If the two numbers differ by the given number of ULPs
-- or less, this function returns @True@.
within :: Int                   -- ^ Number of ULPs of accuracy desired.
       -> Double -> Double -> Bool
within ulps a b
  | ulps < 0  = False
  | otherwise = ulpDistance a b <= fromIntegral ulps
