{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE TypeFamilies       #-}
-- |
-- Module    : Numeric.RootFinding
-- Copyright : (c) 2011 Bryan O'Sullivan, 2018 Alexey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Haskell functions for finding the roots of real functions of real
-- arguments. These algorithms are iterative so we provide both
-- function returning root (or failure to find root) and list of
-- iterations.
module Numeric.RootFinding
    ( -- * Data types
      Root(..)
    , fromRoot
    , Tolerance(..)
    , withinTolerance
    , IterationStep(..)
    , findRoot
    -- * Ridders algorithm
    , RiddersParam(..)
    , ridders
    , riddersIterations
    , RiddersStep(..)
    -- * Newton-Raphson algorithm
    , NewtonParam(..)
    , newtonRaphson
    , newtonRaphsonIterations
    , NewtonStep(..)
    -- * References
    -- $references
    ) where

import Control.Applicative              (Alternative(..))
import Control.Monad                    (MonadPlus(..), ap)
import Control.DeepSeq                  (NFData(..))
import Data.Data                        (Data, Typeable)
import Data.Default.Class
import GHC.Generics                     (Generic)
import Numeric.MathFunctions.Comparison (within,eqRelErr)
import Numeric.MathFunctions.Constants  (m_epsilon)



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | The result of searching for a root of a mathematical function.
data Root a
  = NotBracketed
    -- ^ The function does not have opposite signs when
    -- evaluated at the lower and upper bounds of the search.
  | SearchFailed
    -- ^ The search failed to converge to within the given
    -- error tolerance after the given number of iterations.
  | Root !a
    -- ^ A root was successfully found.
  deriving (Eq, Read, Show, Typeable, Data, Foldable, Traversable, Functor, Generic)

instance (NFData a) => NFData (Root a) where
    rnf NotBracketed = ()
    rnf SearchFailed = ()
    rnf (Root a)     = rnf a

instance Applicative Root where
    pure  = Root
    (<*>) = ap

instance Monad Root where
    NotBracketed >>= _ = NotBracketed
    SearchFailed >>= _ = SearchFailed
    Root a       >>= f = f a
    return = pure

instance MonadPlus Root where
    mzero = empty
    mplus = (<|>)

instance Alternative Root where
    empty = NotBracketed
    r@Root{}     <|> _            = r
    _            <|> r@Root{}     = r
    NotBracketed <|> r            = r
    r            <|> NotBracketed = r
    _            <|> r            = r

-- | Returns either the result of a search for a root, or the default
-- value if the search failed.
fromRoot :: a                 -- ^ Default value.
         -> Root a            -- ^ Result of search for a root.
         -> a
fromRoot _ (Root a) = a
fromRoot a _        = a


-- | Error tolerance for finding root. It describes when root finding
--   algorithm should stop trying to improve approximation.
data Tolerance
  = RelTol !Double
    -- ^ Relative error tolerance. Given @RelTol ε@ two values are
    --   considered approximately equal if
    --   \[ \frac{|a - b|}{|\operatorname{max}(a,b)} < \varepsilon \]
  | AbsTol !Double
    -- ^ Absolute error tolerance. Given @AbsTol δ@ two values are
    --   considered approximately equal if \[ |a - b| < \delta \].
    --   Note that @AbsTol 0@ could be used to require to find
    --   approximation within machine precision.
  deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | Check that two values are approximately equal. In addition to
--   specification values are considered equal if they're within 1ulp
--   of precision. No further improvement could be done anyway.
withinTolerance :: Tolerance -> Double -> Double -> Bool
withinTolerance _ a b
  | within 1 a b = True
withinTolerance (RelTol eps) a b = eqRelErr eps a b
withinTolerance (AbsTol tol) a b = abs (a - b) < tol

-- | Type class for checking whether iteration converged already.
class IterationStep a where
  -- | Return @Just root@ is current iteration converged within
  --   required error tolerance. Returns @Nothing@ otherwise.
  matchRoot :: Tolerance -> a -> Maybe (Root Double)

-- | Find root in lazy list of iterations.
findRoot :: IterationStep a
  => Int                        -- ^ Maximum
  -> Tolerance                  -- ^ Error tolerance
  -> [a]
  -> Root Double
findRoot maxN tol = go 0
  where
    go !i _  | i >= maxN = SearchFailed
    go !_ []             = SearchFailed
    go  i (x:xs)  = case matchRoot tol x of
      Just r  -> r
      Nothing -> go (i+1) xs
{-# INLINABLE  findRoot #-}
{-# SPECIALIZE findRoot :: Int -> Tolerance -> [RiddersStep] -> Root Double #-}
{-# SPECIALIZE findRoot :: Int -> Tolerance -> [NewtonStep]  -> Root Double #-}


----------------------------------------------------------------
-- Attaching information to roots
----------------------------------------------------------------

-- | Parameters for 'ridders' root finding
data RiddersParam = RiddersParam
  { riddersMaxIter :: !Int
    -- ^ Maximum number of iterations. Default = 100
  , riddersTol     :: !Tolerance
    -- ^ Error tolerance for root approximation. Default is relative
    --   error 4·ε, where ε is machine precision.
  }
  deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Default RiddersParam where
  def = RiddersParam
        { riddersMaxIter = 100
        , riddersTol     = RelTol (4 * m_epsilon)
        }

-- | Single Ridders step. It's a bracket of root
data RiddersStep
  = RiddersStep   !Double !Double
  -- ^ Ridders step. Parameters are bracket for the root
  | RiddersBisect !Double !Double
  -- ^ Bisection step. It's fallback which is taken when Ridders
  --   update takes us out of bracket
  | RiddersRoot   !Double
  -- ^ Root found
  | RiddersNoBracket
  -- ^ Root is not bracketed
  deriving (Eq, Read, Show, Typeable, Data, Generic)

instance NFData RiddersStep where
  rnf x = x `seq` ()

instance IterationStep RiddersStep where
  matchRoot tol r = case r of
    RiddersRoot x               -> Just $ Root x
    RiddersNoBracket            -> Just NotBracketed
    RiddersStep a b
      | withinTolerance tol a b -> Just $ Root ((a + b) / 2)
      | otherwise               -> Nothing
    RiddersBisect a b
      | withinTolerance tol a b -> Just $ Root ((a + b) / 2)
      | otherwise               -> Nothing


-- | Use the method of Ridders[Ridders1979] to compute a root of a
--   function. It doesn't require derivative and provide quadratic
--   convergence (number of significant digits grows quadratically
--   with number of iterations).
--
--   The function must have opposite signs when evaluated at the lower
--   and upper bounds of the search (i.e. the root must be
--   bracketed). If there's more that one root in the bracket
--   iteration will converge to some root in the bracket.
ridders
  :: RiddersParam               -- ^ Parameters for algorithms. @def@
                                --   provides reasonable defaults
  -> (Double,Double)            -- ^ Bracket for root
  -> (Double -> Double)         -- ^ Function to find roots
  -> Root Double
ridders p bracket fun
  = findRoot (riddersMaxIter p) (riddersTol p)
  $ riddersIterations bracket fun

-- | List of iterations for Ridders methods. See 'ridders' for
--   documentation of parameters
riddersIterations :: (Double,Double) -> (Double -> Double) -> [RiddersStep]
riddersIterations (lo,hi) f
  | flo == 0    = [RiddersRoot lo]
  | fhi == 0    = [RiddersRoot hi]
    -- root is not bracketed
  | flo*fhi > 0 = [RiddersNoBracket]
    -- Ensure that a<b in iterations
  | lo < hi     = RiddersStep lo hi : go lo flo hi fhi
  | otherwise   = RiddersStep lo hi : go hi fhi lo flo
  where
    flo = f lo
    fhi = f hi
    --
    go !a !fa !b !fb
      | fm == 0       = [RiddersRoot m]
      | fn == 0       = [RiddersRoot n]
      -- Ridder's approximation coincide with one of old bounds or
      -- went out of (a,b) range due to numerical problems. Revert
      -- to bisection
      | n <= a || n >= b   = case () of
          _| fm*fa < 0 -> recBisect a fa m fm
           | otherwise -> recBisect m fm b fb
      | fn*fm < 0          = recRidders n fn m fm
      | fn*fa < 0          = recRidders a fa n fn
      | otherwise          = recRidders n fn b fb
      where
        recBisect  x fx y fy = RiddersBisect x y : go x fx y fy
        recRidders x fx y fy = RiddersStep   x y : go x fx y fy
        --
        dm  = (b - a) * 0.5
        -- Mean point
        m   = (a + b) / 2
        fm  = f m
        -- Ridders update
        n   = m - signum (fb - fa) * dm * fm / sqrt(fm*fm - fa*fb)
        fn  = f n



----------------------------------------------------------------
-- Newton-Raphson algorithm
----------------------------------------------------------------

-- | Parameters for 'ridders' root finding
data NewtonParam = NewtonParam
  { newtonMaxIter :: !Int
    -- ^ Maximum number of iterations. Default = 50
  , newtonTol     :: !Tolerance
    -- ^ Error tolerance for root approximation. Default is relative
    --   error 4·ε, where ε is machine precision
  }
  deriving (Eq, Read, Show, Typeable, Data, Generic)

instance Default NewtonParam where
  def = NewtonParam
        { newtonMaxIter = 50
        , newtonTol     = RelTol (4 * m_epsilon)
        }

-- | Steps for Newton iterations
data NewtonStep
  = NewtonStep         !Double !Double
  -- ^ Normal Newton-Raphson update. Parameters are: old guess, new guess
  | NewtonBisection    !Double !Double
  -- ^ Bisection fallback when Newton-Raphson iteration doesn't
  --   work. Parameters are bracket on root
  | NewtonRoot         !Double
  -- ^ Root is found
  | NewtonNoBracket
  -- ^ Root is not bracketed
  deriving (Eq, Read, Show, Typeable, Data, Generic)

instance NFData NewtonStep where
  rnf x = x `seq` ()

instance IterationStep NewtonStep where
  matchRoot tol r = case r of
    NewtonRoot x                 -> Just (Root x)
    NewtonNoBracket              -> Just NotBracketed
    NewtonStep x x'
      | withinTolerance tol x x' -> Just (Root x')
      | otherwise                -> Nothing
    NewtonBisection a b
      | withinTolerance tol a b  -> Just (Root ((a + b) / 2))
      | otherwise                -> Nothing
  {-# INLINE matchRoot #-}


-- | Solve equation using Newton-Raphson iterations.
--
--   This method require both initial guess and bounds for root. If
--   Newton step takes us out of bounds on root function reverts to
--   bisection.
newtonRaphson
  :: NewtonParam                 -- ^ Parameters for algorithm. @def@
                                 --   provide reasonable defaults.
  -> (Double,Double,Double)      -- ^ Triple of @(low bound, initial
                                 --   guess, upper bound)@. If initial
                                 --   guess if out of bracket middle
                                 --   of bracket is taken as
                                 --   approximation
  -> (Double -> (Double,Double)) -- ^ Function to find root of. It
                                 --   returns pair of function value and
                                 --   its first derivative
  -> Root Double
newtonRaphson p guess fun
  = findRoot (newtonMaxIter p) (newtonTol p)
  $ newtonRaphsonIterations guess fun

-- | List of iteration for Newton-Raphson algorithm. See documentation
--   for 'newtonRaphson' for meaning of parameters.
newtonRaphsonIterations :: (Double,Double,Double) -> (Double -> (Double,Double)) -> [NewtonStep]
newtonRaphsonIterations (lo,guess,hi) function
  | flo == 0    = [NewtonRoot lo]
  | fhi == 0    = [NewtonRoot hi]
  | flo*fhi > 0 = [NewtonNoBracket]
    -- Ensure that function value on low bound is negative
  | flo > 0     = go hi guess' lo
  | otherwise   = go lo guess hi
  where
    (flo,_) = function lo
    (fhi,_) = function hi
    -- Ensure that initial guess is within bracket
    guess'
      | guess >= lo && guess <= hi = guess
      | guess >= hi && guess <= lo = guess
      | otherwise                  = (lo + hi) / 2
    -- Newton iterations. Invariant:
    --   > f xA < 0
    --   > f xB > 0
    go xA x xB
      | f  == 0   = [NewtonRoot x]
      | f' == 0   = bisectionStep
      -- Accept Newton step since it stays within bracket.
      | (x' - xA) * (x' - xB) < 0 = newtonStep
      -- Otherwise bracket root and pick new approximation as
      -- midpoint.
      | otherwise                 = bisectionStep
      where
        -- Calculate Newton step
        (f,f') = function x
        x'   = x - f / f'
        -- Newton step
        newtonStep
          | f > 0     = NewtonStep x x' : go xA x' x
          | otherwise = NewtonStep x x' : go x  x' xB
        -- Fallback bisection step
        bisectionStep
          | f > 0     = NewtonBisection xA x : go xA ((xA + x) / 2) x
          | otherwise = NewtonBisection x xB : go x  ((x + xB) / 2) xB



----------------------------------------------------------------
-- Internal functions
----------------------------------------------------------------

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
