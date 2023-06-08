{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Functions which have different implementations on different platforms
module Numeric.SpecFunctions.Compat (
    erf
  , erfc
  , log1p
  , expm1
  ) where

import Control.Applicative
import qualified Data.Vector.Unboxed as U
import Numeric.MathFunctions.Constants
import Numeric.Polynomial.Chebyshev    (chebyshev,chebyshevBroucke)
import Numeric.Polynomial              (evaluateOddPolynomial)
import Numeric.Series

-- GHC.Float provides log1p and expm1 since base-4.9.0 (GHC8.0). GHCJS
-- doesn't
#if !defined(__GHCJS__)
import GHC.Float (log1p,expm1)
#endif


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
erf x
  -- Computing erf as 1-erfc loses precision near 0 so we switch to
  -- Taylor expansion here
  | abs x < 0.1 = 0.56418958354775629
                * evaluateOddPolynomial x erfTaylorSeries
  | x < 0       = (-1) + erfcCheb (-x)
  | otherwise   =   1  - erfcCheb x

erfTaylorSeries :: U.Vector Double
{-# NOINLINE erfTaylorSeries #-}
erfTaylorSeries = U.fromList
  [  2
  , -2/3
  ,  1/5
  , -1/21
  ,  1/108
  , -1/660
  ,  1/4680
  ]

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


----------------------------------------------------------------
-- expm1 & log1p
--
-- We use one provided by base of for GHCJS use hand-coded one
----------------------------------------------------------------

#if defined(__GHCJS__)
-- | Compute @exp x - 1@ without loss of accuracy for x near zero.
expm1 :: Double -> Double
-- NOTE: this is simplest implementation and not terribly efficient.
expm1 x
  | x < (-37.42994775023705) = -1
  | x > m_max_log            = m_pos_inf
  | abs x > 0.5              = exp x - 1
  | otherwise                = sumSeries $ liftA2 (*) (scanSequence (*) x (pure x))
                                                      (1 / scanSequence (*) 1 (enumSequenceFrom 2))
-- | Compute the natural logarithm of 1 + @x@.  This is accurate even
--   for values of @x@ near zero, where use of @log(1+x)@ would lose
--   precision.
log1p :: Double -> Double
log1p x
    | x == 0               = 0
    | x == -1              = m_neg_inf
    | x < -1               = m_NaN
    | x' < m_epsilon * 0.5 = x
    | (x >= 0 && x < 1e-8) || (x >= -1e-9 && x < 0)
                           = x * (1 - x * 0.5)
    | x' < 0.375           = x * (1 - x * chebyshevBroucke (x / 0.375) coeffs)
    | otherwise            = log (1 + x)
  where
    x' = abs x
    coeffs = U.fromList [
               0.10378693562743769800686267719098e+1,
              -0.13364301504908918098766041553133e+0,
               0.19408249135520563357926199374750e-1,
              -0.30107551127535777690376537776592e-2,
               0.48694614797154850090456366509137e-3,
              -0.81054881893175356066809943008622e-4,
               0.13778847799559524782938251496059e-4,
              -0.23802210894358970251369992914935e-5,
               0.41640416213865183476391859901989e-6,
              -0.73595828378075994984266837031998e-7,
               0.13117611876241674949152294345011e-7,
              -0.23546709317742425136696092330175e-8,
               0.42522773276034997775638052962567e-9,
              -0.77190894134840796826108107493300e-10,
               0.14075746481359069909215356472191e-10,
              -0.25769072058024680627537078627584e-11,
               0.47342406666294421849154395005938e-12,
              -0.87249012674742641745301263292675e-13,
               0.16124614902740551465739833119115e-13,
              -0.29875652015665773006710792416815e-14,
               0.55480701209082887983041321697279e-15,
              -0.10324619158271569595141333961932e-15
             ]
#endif
