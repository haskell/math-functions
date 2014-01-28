{-# LANGUAGE BangPatterns, DeriveDataTypeable, MultiParamTypeClasses,
    TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |
-- Module    : Numeric.Sum
-- Copyright : (c) 2013 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for summing floating point numbers more accurately than
-- the standard 'sum' function.
--
-- When used with floating point numbers, in the worst case, the 'sum'
-- function accumulates numeric error at a rate proportional to the
-- number of values being summed. The algorithms in this module
-- implement different methods of /compensated summation/, which
-- reduce the accumulation of numeric error so that it either grows
-- much more slowly than the number of inputs (e.g. logarithmically),
-- or remains constant.
module Numeric.Sum (
      Summation(..)
    -- Kahan summation
    , KahanSum(..)
    , kahan
    -- Kahan-Babuška-Neumaier summation
    , KBNSum(..)
    , kbn
    -- Order-2 Kahan-Babuška summation
    , KB2Sum(..)
    , kb2
    -- * References
    -- $references
    ) where

import Data.Data (Typeable, Data)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Data.Foldable as F

class Summation s where
    zero :: s
    add  :: s -> Double -> s

    sum  :: (F.Foldable f) => (s -> Double) -> f Double -> Double
    sum  f = f . F.foldl' add zero
    {-# INLINE sum #-}

instance Summation Double where
    zero = 0
    add = (+)

data KahanSum = KahanSum {-# UNPACK #-} !Double {-# UNPACK #-} !Double
              deriving (Eq, Show, Typeable, Data)

derivingUnbox "KahanSum"
    [t| KahanSum -> (Double, Double) |]
    [| \ (KahanSum a b) -> (a, b) |]
    [| \ (a, b) -> KahanSum a b |]

instance Summation KahanSum where
    zero = KahanSum 0 0
    add  = kahanAdd

kahanAdd :: KahanSum -> Double -> KahanSum
kahanAdd (KahanSum sum c) x = KahanSum sum' c'
  where sum' = sum + y
        c'   = (sum' - sum) - y
        y    = x - c

kahan :: KahanSum -> Double
kahan (KahanSum sum _) = sum

data KBNSum = KBNSum {-# UNPACK #-} !Double {-# UNPACK #-} !Double
            deriving (Eq, Show, Typeable, Data)

derivingUnbox "KBNSum"
    [t| KBNSum -> (Double, Double) |]
    [| \ (KBNSum a b) -> (a, b) |]
    [| \ (a, b) -> KBNSum a b |]

instance Summation KBNSum where
    zero = KBNSum 0 0
    add  = kbnAdd

kbnAdd :: KBNSum -> Double -> KBNSum
kbnAdd (KBNSum sum c) x = KBNSum sum' c'
  where c' | abs sum >= abs x = c + ((sum - sum') + x)
           | otherwise        = c + ((x - sum') + sum)
        sum'                  = sum + x

kbn :: KBNSum -> Double
kbn (KBNSum sum c) = sum + c

data KB2Sum = KB2Sum {-# UNPACK #-} !Double
                     {-# UNPACK #-} !Double
                     {-# UNPACK #-} !Double
            deriving (Eq, Show, Typeable, Data)

derivingUnbox "KB2Sum"
    [t| KB2Sum -> (Double, Double, Double) |]
    [| \ (KB2Sum a b c) -> (a, b, c) |]
    [| \ (a, b, c) -> KB2Sum a b c |]

instance Summation KB2Sum where
    zero = KB2Sum 0 0 0
    add  = kb2Add

kb2Add :: KB2Sum -> Double -> KB2Sum
kb2Add (KB2Sum sum c cc) x = KB2Sum sum' c' cc'
  where sum'                 = sum + x
        c'                   = c + k
        cc' | abs c >= abs k = cc + ((c - c') + k)
            | otherwise      = cc + ((k - c') + c)
        k | abs sum >= abs x = (sum - sum') + x
          | otherwise        = (x - sum') + sum

kb2 :: KB2Sum -> Double
kb2 (KB2Sum sum c cc) = sum + c + cc


-- $references
--
-- * Kahan, W. (1965), Further remarks on reducing truncation
--   errors. /Communications of the ACM/ 8(1):40.
--
-- * Neumaier, A. (1974), Rundungsfehleranalyse einiger Verfahren zur
--   Summation endlicher Summen. /Zeitschrift für Angewandte
--   Mathematik und Mechanik/ 54:39–51.
--
-- * Klein, A. (2006), A Generalized
--   Kahan-Babuška-Summation-Algorithm. /Computing/ 76(3):279-293.
