{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, CPP #-}
-- |
-- Module    : Numeric.RootFinding
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Haskell functions for finding the roots of real functions of real arguments.
module Numeric.RootFinding
    ( -- * Data types
      Root(..)
    , fromRoot
    , Tolerance(..)
    , withinTolerance
    -- * Ridders algorithm
    , RiddersParam(..)
    , ridders
    -- * Newton-Raphson algorithm
    , NewtonParam(..)
    , newtonRaphson
    -- * References
    -- $references
    ) where

import Control.Applicative              (Alternative(..), Applicative(..))
import Control.Monad                    (MonadPlus(..), ap)
import Data.Data                        (Data, Typeable)
import Data.Default.Class
#if __GLASGOW_HASKELL__ > 704
import GHC.Generics                     (Generic)
#endif
import Numeric.MathFunctions.Comparison (within,eqRelErr)
import Numeric.MathFunctions.Constants  (m_epsilon)


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | The result of searching for a root of a mathematical function.
data Root a = NotBracketed
            -- ^ The function does not have opposite signs when
            -- evaluated at the lower and upper bounds of the search.
            | SearchFailed
            -- ^ The search failed to converge to within the given
            -- error tolerance after the given number of iterations.
            | Root a
            -- ^ A root was successfully found.
              deriving (Eq, Read, Show, Typeable, Data
#if __GLASGOW_HASKELL__ > 704
                       , Generic
#endif
                       )


instance Functor Root where
    fmap _ NotBracketed = NotBracketed
    fmap _ SearchFailed = SearchFailed
    fmap f (Root a)     = Root (f a)

instance Monad Root where
    NotBracketed >>= _ = NotBracketed
    SearchFailed >>= _ = SearchFailed
    Root a       >>= m = m a

    return = Root

instance MonadPlus Root where
    mzero = SearchFailed

    r@(Root _) `mplus` _ = r
    _          `mplus` p = p

instance Applicative Root where
    pure  = Root
    (<*>) = ap

instance Alternative Root where
    empty = SearchFailed

    r@(Root _) <|> _ = r
    _          <|> p = p

-- | Returns either the result of a search for a root, or the default
-- value if the search failed.
fromRoot :: a                   -- ^ Default value.
         -> Root a              -- ^ Result of search for a root.
         -> a
fromRoot _ (Root a) = a
fromRoot a _        = a


-- | Error tolerance for finding root. It describes when root finding
--   algorithm should stop trying to improve approximation.
data Tolerance
  = RelTol !Double
    -- ^ Relative error tolerance. Given @RelTol ε@ two values are
    --   considered approximately equal if
    --   \[ |a - b| / |\operatorname{max}(a,b)} < \vareps \]
  | AbsTol !Double
    -- ^ Absolute error tolerance. Given @AbsTol δ@ two values are
    --   considered approximately equal if \[ |a - b| < \delta \].
    --   Note that @AbsTol 0@ could be used to require to find
    --   approximation within machine precision.
  deriving (Eq, Read, Show, Typeable, Data
#if __GLASGOW_HASKELL__ > 704
           , Generic
#endif
           )

withinTolerance :: Tolerance -> Double -> Double -> Bool
withinTolerance (RelTol eps) a b = eqRelErr eps a b
-- NOTE: `<=` is needed to allow 0 absolute tolerance which is used to
--       describe precision of 1ulp
withinTolerance (AbsTol tol) a b = abs (a - b) <= tol

----------------------------------------------------------------
-- Ridders algorithm
----------------------------------------------------------------

-- | Parameters for 'ridders' root finding
data RiddersParam = RiddersParam
  { riddersMaxIter :: !Int
    -- ^ Maximum number of iterations.
  , riddersTol     :: !Tolerance
    -- ^ Error tolerance for root approximation.
  }
  deriving (Eq, Read, Show, Typeable, Data
#if __GLASGOW_HASKELL__ > 704
           , Generic
#endif
           )

instance Default RiddersParam where
  def = RiddersParam
        { riddersMaxIter = 100
        , riddersTol     = RelTol (4 * m_epsilon)
        }


-- | Use the method of Ridders[Ridders1979] to compute a root of a
--   function. It doesn't require derivative and provide quadratic
--   convergence (number of significant digits grows quadratically
--   with number of iterations).
--
--   The function must have opposite signs when evaluated at the lower
--   and upper bounds of the search (i.e. the root must be
--   bracketed). If there's more that one root in the bracket
--   iteration will converge to some root in the bracket.
ridders :: RiddersParam
        -- ^ Absolute error tolerance. Iterations will be stopped when
        --   difference between root and estimate is less than
        --   tolerance or when precision couldn't be improved further
        --   (root is within 1 ulp).
        -> (Double,Double)
        -- ^ Lower and upper bounds for the search.
        -> (Double -> Double)
        -- ^ Function to find the roots of.
        -> Root Double
ridders p (lo,hi) f
    | flo == 0    = Root lo
    | fhi == 0    = Root hi
    -- root is not bracketed
    | flo*fhi > 0 = NotBracketed
    -- Ensure that a<b in iterations
    | lo < hi     = go lo flo hi fhi 0
    | otherwise   = go hi fhi lo flo 0
  where
    !flo = f lo
    !fhi = f hi
    --
    go !a !fa !b !fb !i
        -- Root is bracketed within 1 ulp. No improvement could be made
        | within 1 a b = Root a
        -- Root is found. Check that f(m) == 0 is nessesary to ensure
        -- that root is never passed to 'go'
        | fm == 0      = Root m
        | fn == 0      = Root n
        | withinTolerance (riddersTol p) a b = Root n
        -- Too many iterations performed. Fail
        | i >= riddersMaxIter p              = SearchFailed
        -- Ridder's approximation coincide with one of old bounds or
        -- went out of (a,b) range due to numerical problems. Revert
        -- to bisection
        | n <= a || n >= b   = case () of
          _| fm*fa < 0 -> go a fa m fm (i+1)
           | otherwise -> go m fm b fb (i+1)
        -- Proceed as usual
        | fn*fm < 0          = go n fn m fm (i+1)
        | fn*fa < 0          = go a fa n fn (i+1)
        | otherwise          = go n fn b fb (i+1)
      where
        dm   = (b - a) * 0.5
        -- Mean point
        !m   = a + dm
        !fm  = f m
        -- Ridders update
        !n   = m - signum (fb - fa) * dm * fm / sqrt(fm*fm - fa*fb)
        !fn  = f n



----------------------------------------------------------------
-- Newton-Raphson algorithm
----------------------------------------------------------------

-- | Parameters for 'ridders' root finding
data NewtonParam = NewtonParam
  { newtonMaxIter :: !Int
    -- ^ Maximum number of iterations.
  , newtonTol     :: !Tolerance
    -- ^ Error tolerance for root approximation.
  }
  deriving (Eq, Read, Show, Typeable, Data
#if __GLASGOW_HASKELL__ > 704
           , Generic
#endif
           )

instance Default NewtonParam where
  def = NewtonParam
        { newtonMaxIter = 50
        , newtonTol     = RelTol (4 * m_epsilon)
        }


-- | Solve equation using Newton-Raphson iterations.
--
-- This method require both initial guess and bounds for root. If
-- Newton step takes us out of bounds on root function reverts to
-- bisection.
newtonRaphson
  :: NewtonParam
  -- ^ Required precision
  -> (Double,Double,Double)
  -- ^ (lower bound, initial guess, upper bound). Iterations will no
  -- go outside of the interval
  -> (Double -> (Double,Double))
  -- ^ Function to finds roots. It returns pair of function value and
  -- its derivative
  -> Root Double
newtonRaphson p (!low,!guess,!hi) function
  = go low guess hi 0
  where
    go !xMin !x !xMax !i
      | f  == 0                            = Root x
      | f' == 0                            = SearchFailed
      | withinTolerance (newtonTol p) x' x = Root x'
      | i >= newtonMaxIter p               = SearchFailed
      | otherwise                          = go xMin' x' xMax' (i+1)
      where
        -- Calculate Newton-Raphson step
        (f,f') = function x
        delta  = f / f'
        -- Calculate new approximation and check that we don't go out of the bracket
        (dx,x') | z <= xMin = let d = 0.5*(x - xMin) in (d, x - d)
                | z >= xMax = let d = 0.5*(x - xMax) in (d, x - d)
                | otherwise = (delta, z)
          where z = x - delta
        -- Update root bracket
        xMin' | dx < 0    = x
              | otherwise = xMin
        xMax' | dx > 0    = x
              | otherwise = xMax



-- $references
--
-- * Ridders, C.F.J. (1979) A new algorithm for computing a single
--   root of a real continuous function.
--   /IEEE Transactions on Circuits and Systems/ 26:979&#8211;980.
--
-- * Press W.H.; Teukolsky S.A.; Vetterling W.T.; Flannery B.P.
--   (2007). \"Section 9.2.1. Ridders' Method\". /Numerical Recipes: The
--   Art of Scientific Computing (3rd ed.)./ New York: Cambridge
--   University Press. ISBN 978-0-521-88068-8.
