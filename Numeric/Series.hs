{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Functions for working with series
module Numeric.Series (
    -- * Data type for series
    Series(..)
    -- * Constructors
  , enumSeriesFrom
  , enumSeriesFromStep
  , scanSeries
    -- * Evaluation of series
  , sumSeries
  , sumPowerSeries
  , seriesToList
    -- * Evaluation of continued fractions
  , evalModLentz
  ) where

import Control.Applicative
import Data.List (unfoldr)

import Numeric.MathFunctions.Constants (m_epsilon)


----------------------------------------------------------------

-- | Infinite series. It's represented as opaque state and step
--   function.
data Series a = forall s. Series s (s -> (a,s))

instance Functor Series where
  fmap f (Series s0 step) = Series s0 (\s -> let (a,s') = step s in (f a, s'))
  {-# INLINE fmap #-}

instance Applicative Series where
  pure a = Series () (\() -> (a,()))
  Series sA fA <*> Series sB fB = Series (sA,sB) $ \(!sa,!sb) ->
    let (a,sa') = fA sa
        (b,sb') = fB sb
    in (a b, (sa',sb'))
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

-- | Elementwise operations with series
instance Num a => Num (Series a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  {-# INLINE (+) #-}
  {-# INLINE (*) #-}
  {-# INLINE (-) #-}
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger
  {-# INLINE abs         #-}
  {-# INLINE signum      #-}
  {-# INLINE fromInteger #-}

-- | Elementwise operations with series
instance Fractional a => Fractional (Series a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = pure . fromRational
  {-# INLINE (/)          #-}
  {-# INLINE recip        #-}
  {-# INLINE fromRational #-}



----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------

-- | @enumSeriesFrom x@ generate series:
--
-- \[ a_n = x + n \]
enumSeriesFrom :: Num a => a -> Series a
enumSeriesFrom i = Series i (\n -> (n,n+1))
{-# INLINE enumSeriesFrom #-}

-- | @enumSeriesFromStep x d@ generate series:
--
-- \[ a_n = x + nd \]
enumSeriesFromStep :: Num a => a -> a -> Series a
enumSeriesFromStep n d = Series n (\i -> (i,i+d))
{-# INLINE enumSeriesFromStep #-}

-- | Analog of 'scanl' for series.
scanSeries :: (b -> a -> b) -> b -> Series a -> Series b
{-# INLINE scanSeries #-}
scanSeries f b0 (Series s0 step) = Series (b0,s0) $ \(b,s) ->
  let (a,s') = step s
      b'     = f b a
  in (b,(b',s'))


----------------------------------------------------------------
-- Evaluation of series
----------------------------------------------------------------

-- | Calculate sum of series
--
-- \[ \sum_{i=0}^\infty a_i \]
--
-- Calculation is stopped when next value in series is less than
-- ε·sum.
sumSeries :: Series Double -> Double
{-# INLINE sumSeries #-}
sumSeries (Series sInit step)
  = go x0 s0
  where 
    (x0,s0) = step sInit
    go x s | abs (d/x) < m_epsilon = x'
           | otherwise             = go x' s'
      where (d,s') = step s
            x'     = x + d

-- | Calculate sum of series
--
-- \[ \sum_{i=0}^\infty x^ia_i \]
--
-- Calculation is stopped when next value in series is less than
-- ε·sum.
sumPowerSeries :: Double -> Series Double -> Double
sumPowerSeries x ser = sumSeries $ liftA2 (*) (scanSeries (*) 1 (pure x)) ser
{-# INLINE sumPowerSeries #-}

-- | Convert series to infinite list
seriesToList :: Series a -> [a]
seriesToList (Series s f) = unfoldr (Just . f) s



----------------------------------------------------------------
-- Evaluation of continued fractions
----------------------------------------------------------------

-- |
-- Evaluate continued fraction using modified Lentz algorithm.
-- Series contain pairs (a[i],b[i]) which form following expression:
--
-- \[
-- b_0 + \frac{a_1}{b_1+\frac{a_2}{b_2+\frac{a_3}{b_3 + \cdots}}}
-- \]
--
-- Modified Lentz algorithm is described in Numerical recipes 5.2
-- "Evaluation of Continued Fractions"
evalModLentz :: Series (Double,Double) -> Double
{-# INLINE evalModLentz #-}
evalModLentz (Series sInit step)
  = let ((_,b0),s0) = step sInit
        f0          = maskZero b0
    in  go f0 f0 0 s0
  where
    tiny = 1e-60
    maskZero 0 = tiny
    maskZero x = x
    
    go f c d s
      | abs (delta - 1) < m_epsilon = f'
      | otherwise                   = go f' c' d' s'
      where     
          ((a,b),s') = step s
          d'    = recip $ maskZero $ b + a*d
          c'    = maskZero $ b + a/c 
          delta = c'*d'
          f'    = f*delta
