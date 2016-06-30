{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Code for working with series
module Numeric.Series (
    -- * Data type for series
    Series(..)
  , next
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

-- | Representation of infinite series
data Series a = forall s. Series s (s -> (a,s))

instance Functor Series where
  fmap f (Series s0 step) = Series s0 (\s -> let (a,s') = step s in (f a, s'))

instance Applicative Series where
  pure a = Series () (\_ -> (a,()))
  Series sA fA <*> Series sB fB = Series (sA,sB) $ \(sa,sb) ->
    let (a,sa') = fA sa
        (b,sb') = fB sb
    in (a b, (sa',sb'))

next :: Series a -> (a,Series a)
{-# INLINE next #-}
next (Series s f) = case f s of
  (a,s') -> (a,Series s' f)


----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------0

enumSeriesFrom :: Num a => a -> Series a
enumSeriesFrom i = Series i (\n -> (n,n+1))
{-# INLINE enumSeriesFrom #-}

enumSeriesFromStep :: Num a => a -> a -> Series a
enumSeriesFromStep n d = Series n (\i -> (i,i+d))
{-# INLINE enumSeriesFromStep #-}

scanSeries :: (b -> a -> b) -> b -> Series a -> Series b
{-# INLINE scanSeries #-}
scanSeries f b0 (Series s0 step) = Series (b0,s0) $ \(b,s) ->
  let (a,s') = step s
      b'     = f b a
  in (b,(b',s'))


----------------------------------------------------------------
-- Evaluation of series
----------------------------------------------------------------

sumSeries :: Series Double -> Double
sumSeries ser0
  = go x0 ser
  where 
    (x0,ser) = next ser0
    go x s | abs (d/x) < m_epsilon = x'
           | otherwise             = go x' s'
      where (d,s') = next s
            x' = x + d

sumPowerSeries :: Double -> Series Double -> Double
sumPowerSeries x ser = sumSeries $ liftA2 (*) (scanSeries (*) 1 (pure x)) ser
            
seriesToList :: Series a -> [a]
seriesToList (Series s f) = unfoldr (Just . f) s



----------------------------------------------------------------
-- Evaluation of continued fractions
----------------------------------------------------------------

evalModLentz :: Series (Double,Double) -> Double
evalModLentz ser0
  = let ((_,b0), ser) = next ser0
        f0            = maskZero b0
    in  go f0 f0 0 ser
  where
    tiny = 1e-60
    maskZero 0 = tiny
    maskZero x = x
    
    go f c d ser
      | abs (delta - 1) < m_epsilon = f'
      | otherwise                   = go f' c' d' ser'
      where     
          ((a,b),ser') = next ser
          d'    = recip $ maskZero $ b + a*d
          c'    = maskZero $ b + a/c 
          delta = c'*d'
          f'    = f*delta
