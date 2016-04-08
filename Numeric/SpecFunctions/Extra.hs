-- |
-- Module    : Numeric.SpecFunctions.Extra
-- Copyright : (c) 2009, 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Less common mathematical functions.
module Numeric.SpecFunctions.Extra (
    bd0
  , chooseExact
  , logChooseFast
  ) where

import Numeric.MathFunctions.Constants (m_NaN)
import Numeric.SpecFunctions.Internal  (chooseExact,logChooseFast)

-- | Evaluate the deviance term @x log(x/np) + np - x@.
bd0 :: Double                   -- ^ @x@
    -> Double                   -- ^ @np@
    -> Double 
bd0 x np 
  | isInfinite x || isInfinite np || np == 0 = m_NaN
  | abs x_np >= 0.1*(x+np)                   = x * log (x/np) - x_np
  | otherwise                                = loop 1 (ej0*vv) s0
  where 
    x_np = x - np
    v    = x_np / (x+np)
    s0   = x_np * v
    ej0  = 2*x*v
    vv   = v*v
    loop j ej s = case s + ej/(2*j+1) of
                    s' | s' == s   -> s'  -- FIXME: Comparing Doubles for equality!
                       | otherwise -> loop (j+1) (ej*vv) s'
