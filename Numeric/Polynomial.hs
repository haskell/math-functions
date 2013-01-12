-- |
-- Module    : Numeric.Polynomial
-- Copyright : (c) 2012 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Function for evaluating polynomials using Horher's method.
module Numeric.Polynomial (
    evaluatePolynomial
  , evaluateEvenPolynomial
  , evaluateOddPolynomial
  ) where

import qualified Data.Vector.Generic as G
import           Data.Vector.Generic  (Vector)


-- | Evaluate polynomial using Horner's method. Coefficients starts
-- from lowest. In pseudocode:
--
-- > evaluateOddPolynomial x [1,2,3] = 1 + 2*x + 3*x^2
evaluatePolynomial :: (Vector v a, Num a)
                   => a    -- ^ /x/
                   -> v a  -- ^ Coefficients
                   -> a
{-# INLINE evaluatePolynomial #-}
evaluatePolynomial x coefs
  = G.foldr (\a r -> a + r*x) 0 coefs

-- | Evaluate polynomial with only even powers using Horner's method.
-- Coefficients starts from lowest. In pseudocode:
--
-- > evaluateOddPolynomial x [1,2,3] = 1 + 2*x^2 + 3*x^4
evaluateEvenPolynomial :: (Vector v a, Num a)
                       => a    -- ^ /x/
                       -> v a  -- ^ Coefficients
                       -> a
{-# INLINE evaluateEvenPolynomial #-}
evaluateEvenPolynomial x coefs
  = G.foldr (\a r -> a + r*x2) 0 coefs
  where x2 = x * x

-- | Evaluate polynomial with only odd powers using Horner's method.
-- Coefficients starts from lowest. In pseudocode:
--
-- > evaluateOddPolynomial x [1,2,3] = 1*x + 2*x^3 + 3*x^5
evaluateOddPolynomial :: (Vector v a, Num a)
                       => a    -- ^ /x/
                       -> v a  -- ^ Coefficients
                       -> a
{-# INLINE evaluateOddPolynomial #-}
evaluateOddPolynomial x coefs
  = x * G.foldr (\a r -> a + r*x2) 0 coefs
  where x2 = x * x
