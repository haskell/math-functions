{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Numeric.Polynomial.Chebyshev
-- Copyright : (c) 2012 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Chebyshev polynomials.
module Numeric.Polynomial (
    evaluatePolynomial
  , evaluateEvenPolynomial
  , evaluateOddPolynomial
  ) where

import qualified Data.Vector.Generic as G
import           Data.Vector.Generic  (Vector)


-- | Evaluate polynomial using Horner's method.
evaluatePolynomial :: (Vector v Double)
                   => Double    -- ^ /x/
                   -> v Double  -- ^ Coefficients
                   -> Double
{-# INLINE evaluatePolynomial #-}
evaluatePolynomial x coefs
  = G.foldr (\a r -> a + r*x) 0 coefs

-- | Evaluate polynomial with event powers only using Horner's method
evaluateEvenPolynomial :: (Vector v Double)
                       => Double    -- ^ /x/
                       -> v Double  -- ^ Coefficients
                       -> Double
{-# INLINE evaluateEvenPolynomial #-}
evaluateEvenPolynomial x coefs
  = G.foldr (\a r -> a + r*x2) 0 coefs
  where x2 = x * x

-- | Evaluate polynomial with only powers only using Horner's method
evaluateOddPolynomial :: (Vector v Double)
                       => Double    -- ^ /x/
                       -> v Double  -- ^ Coefficients
                       -> Double
{-# INLINE evaluateOddPolynomial #-}
evaluateOddPolynomial x coefs
  = x * G.foldr (\a r -> a + r*x2) 0 coefs
  where x2 = x * x

