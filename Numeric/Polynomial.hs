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
