{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Functions which have different implementations on different platforms
module Numeric.SpecFunctions.Compat (
    erf
  , erfc
  ) where

import qualified Data.Vector.Unboxed as U

import Numeric.Polynomial.Chebyshev    (chebyshev)

-- GHC.Float provides log1p and expm1 since base-4.9.0 (GHC8.0)
#define USE_GHC_LOG1P_EXP1M (MIN_VERSION_base(4,9,0) && !defined(__GHCJS__))


----------------------------------------------------------------
-- erf & erfc
--
-- We provide pure haskell implementation for GHCJS and accesible on
-- GHC via flag
----------------------------------------------------------------

#if USE_SYSTEM_ERF && !defined(__GHCJS__)

erf :: Double -> Double
erf = c_erf
{-# INLINE erf #-}

erfc :: Double -> Double
erfc = c_erfc
{-# INLINE erfc #-}

foreign import ccall unsafe "erf"  c_erf  :: Double -> Double
foreign import ccall unsafe "erfc" c_erfc :: Double -> Double

#else

erf :: Double -> Double
erf x | x < 0     = (-1) + erfcCheb (-x)
      | otherwise =   1  - erfcCheb x

erfc :: Double -> Double
erfc x | x < 0     = 2 - erfcCheb (-x)
       | otherwise = erfcCheb x

-- Adapted from Numerical Recipes §6.2.2
erfcCheb :: Double -> Double
erfcCheb z
  = t * exp( -z * z + chebyshev ty erfcCoef )
  where
    -- We're using approximation:
    --
    --   erfc(z) ≈ t·exp(-z² + P(t))
    --   t       = 2 / (2 + z)
    t  = 2 / (2 + z)
    ty = 2 * t - 1

erfcCoef :: U.Vector Double
{-# NOINLINE erfcCoef #-}
erfcCoef = U.fromList
  [ -0.6513268598908546   ,  6.4196979235649026e-1 ,  1.9476473204185836e-2
  , -9.561514786808631e-3 , -9.46595344482036e-4   ,  3.66839497852761e-4
  ,  4.2523324806907e-5   , -2.0278578112534e-5    , -1.624290004647e-6
  ,  1.303655835580e-6    ,  1.5626441722e-8       , -8.5238095915e-8
  ,  6.529054439e-9       ,  5.059343495e-9        , -9.91364156e-10
  , -2.27365122e-10       ,  9.6467911e-11         ,  2.394038e-12
  , -6.886027e-12         ,  8.94487e-13           ,  3.13092e-13
  , -1.12708e-13          ,  3.81e-16              ,  7.106e-15
  , -1.523e-15            , -9.4e-17               ,  1.21e-16
  , -2.8e-17
  ]

#endif
