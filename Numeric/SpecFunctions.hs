{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
-- |
-- Module    : Numeric.SpecFunctions
-- Copyright : (c) 2009, 2011, 2012 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Special functions and factorials.
module Numeric.SpecFunctions (
    -- * Error function
    erf
  , erfc
  , invErf
  , invErfc
    -- * Gamma function
  , logGamma
  , logGammaL
  , incompleteGamma
  , invIncompleteGamma
  , digamma
    -- * Beta function
  , logBeta
  , incompleteBeta
  , incompleteBeta_
  , invIncompleteBeta
    -- * Logarithm
  , log1p
  , log2
    -- * Factorial
  , factorial
  , logFactorial
  , stirlingError
    -- * Combinatorics
  , choose
    -- * References
    -- $references
  ) where

import Data.Bits       ((.&.), (.|.), shiftR)
import Data.Int        (Int64)
import qualified Data.Number.Erf     as Erf (erfc,erf)
import qualified Data.Vector.Unboxed as U

import Numeric.Polynomial.Chebyshev    (chebyshevBroucke)
import Numeric.Polynomial              (evaluateEvenPolynomialL,evaluateOddPolynomialL)
import Numeric.MathFunctions.Constants ( m_epsilon, m_NaN, m_neg_inf, m_pos_inf
                                       , m_sqrt_2_pi, m_ln_sqrt_2_pi, m_sqrt_2
                                       , m_eulerMascheroni
                                       )
import Text.Printf


----------------------------------------------------------------
-- Error function
----------------------------------------------------------------

-- | Error function.
--
-- > erf -∞ = -1
-- > erf  0 =  0
-- > erf +∞ =  1
erf :: Double -> Double
{-# INLINE erf #-}
erf = Erf.erf

-- | Complementary error function.
--
-- > erfc -∞ = 2
-- > erfc  0 = 1
-- > errc +∞ = 0
erfc :: Double -> Double
{-# INLINE erfc #-}
erfc = Erf.erfc


-- | Inverse of 'erf'.
invErf :: Double -- ^ /p/ ∈ [-1,1]
       -> Double
invErf p = invErfc (1 - p)

-- | Inverse of 'erfc'.
invErfc :: Double -- ^ /p/ ∈ [0,2]
        -> Double
invErfc p
  | p == 2        = m_neg_inf
  | p == 0        = m_pos_inf
  | p >0 && p < 2 = if p <= 1 then r else -r
  | otherwise     = modErr $ "invErfc: p must be in [0,2] got " ++ show p
  where
    pp = if p <= 1 then p else 2 - p
    t  = sqrt $ -2 * log( 0.5 * pp)
    -- Initial guess
    x0 = -0.70711 * ((2.30753 + t * 0.27061) / (1 + t * (0.99229 + t * 0.04481)) - t)
    r  = loop 0 x0
    --
    loop :: Int -> Double -> Double
    loop !j !x
      | j >= 2    = x
      | otherwise = let err = erfc x - pp
                        x'  = x + err / (1.12837916709551257 * exp(-x * x) - x * err) -- // Halley
                    in loop (j+1) x'



----------------------------------------------------------------
-- Gamma function
----------------------------------------------------------------

-- Adapted from http://people.sc.fsu.edu/~burkardt/f_src/asa245/asa245.html

-- | Compute the logarithm of the gamma function Γ(/x/).  Uses
-- Algorithm AS 245 by Macleod.
--
-- Gives an accuracy of 10-12 significant decimal digits, except
-- for small regions around /x/ = 1 and /x/ = 2, where the function
-- goes to zero.  For greater accuracy, use 'logGammaL'.
--
-- Returns ∞ if the input is outside of the range (0 < /x/ ≤ 1e305).
logGamma :: Double -> Double
logGamma x
    | x <= 0    = m_pos_inf
    -- Handle positive infinity. logGamma overflows before 1e308 so
    -- it's safe
    | x > 1e308 = m_pos_inf
    -- Normal cases
    | x < 1.5   = a + c *
                  ((((r1_4 * b + r1_3) * b + r1_2) * b + r1_1) * b + r1_0) /
                  ((((b + r1_8) * b + r1_7) * b + r1_6) * b + r1_5)
    | x < 4     = (x - 2) *
                  ((((r2_4 * x + r2_3) * x + r2_2) * x + r2_1) * x + r2_0) /
                  ((((x + r2_8) * x + r2_7) * x + r2_6) * x + r2_5)
    | x < 12    = ((((r3_4 * x + r3_3) * x + r3_2) * x + r3_1) * x + r3_0) /
                  ((((x + r3_8) * x + r3_7) * x + r3_6) * x + r3_5)
    | x > 3e6   = k
    | otherwise = k + x1 *
                  ((r4_2 * x2 + r4_1) * x2 + r4_0) /
                  ((x2 + r4_4) * x2 + r4_3)
  where
    (a , b , c)
        | x < 0.5   = (-y , x + 1 , x)
        | otherwise = (0  , x     , x - 1)

    y      = log x
    k      = x * (y-1) - 0.5 * y + alr2pi
    alr2pi = 0.918938533204673

    x1 = 1 / x
    x2 = x1 * x1

    r1_0 =  -2.66685511495;   r1_1 =  -24.4387534237;    r1_2 = -21.9698958928
    r1_3 =  11.1667541262;    r1_4 =    3.13060547623;   r1_5 =   0.607771387771
    r1_6 =  11.9400905721;    r1_7 =   31.4690115749;    r1_8 =  15.2346874070

    r2_0 = -78.3359299449;    r2_1 = -142.046296688;     r2_2 = 137.519416416
    r2_3 =  78.6994924154;    r2_4 =    4.16438922228;   r2_5 =  47.0668766060
    r2_6 = 313.399215894;     r2_7 =  263.505074721;     r2_8 =  43.3400022514

    r3_0 =  -2.12159572323e5; r3_1 =    2.30661510616e5; r3_2 =   2.74647644705e4
    r3_3 =  -4.02621119975e4; r3_4 =   -2.29660729780e3; r3_5 =  -1.16328495004e5
    r3_6 =  -1.46025937511e5; r3_7 =   -2.42357409629e4; r3_8 =  -5.70691009324e2

    r4_0 = 0.279195317918525;  r4_1 = 0.4917317610505968;
    r4_2 = 0.0692910599291889; r4_3 = 3.350343815022304
    r4_4 = 6.012459259764103



data L = L {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Compute the logarithm of the gamma function, &#915;(/x/).  Uses a
-- Lanczos approximation.
--
-- This function is slower than 'logGamma', but gives 14 or more
-- significant decimal digits of accuracy, except around /x/ = 1 and
-- /x/ = 2, where the function goes to zero.
--
-- Returns &#8734; if the input is outside of the range (0 < /x/
-- &#8804; 1e305).
logGammaL :: Double -> Double
logGammaL x
    | x <= 0    = m_pos_inf
    -- Lanroz approximation loses precision for small arguments
    | x <= 1e-3 = logGamma x
    | otherwise = fini . U.foldl' go (L 0 (x+7)) $ a
    where fini (L l _) = log (l+a0) + log m_sqrt_2_pi - x65 + (x-0.5) * log x65
          go (L l t) k = L (l + k / t) (t-1)
          x65 = x + 6.5
          a0  = 0.9999999999995183
          a   = U.fromList [ 0.1659470187408462e-06
                           , 0.9934937113930748e-05
                           , -0.1385710331296526
                           , 12.50734324009056
                           , -176.6150291498386
                           , 771.3234287757674
                           , -1259.139216722289
                           , 676.5203681218835
                           ]



-- | Compute the log gamma correction factor for @x@ &#8805; 10.  This
-- correction factor is suitable for an alternate (but less
-- numerically accurate) definition of 'logGamma':
--
-- >lgg x = 0.5 * log(2*pi) + (x-0.5) * log x - x + logGammaCorrection x
logGammaCorrection :: Double -> Double
logGammaCorrection x
    | x < 10    = m_NaN
    | x < big   = chebyshevBroucke (t * t * 2 - 1) coeffs / x
    | otherwise = 1 / (x * 12)
  where
    big    = 94906265.62425156
    t      = 10 / x
    coeffs = U.fromList [
               0.1666389480451863247205729650822e+0,
              -0.1384948176067563840732986059135e-4,
               0.9810825646924729426157171547487e-8,
              -0.1809129475572494194263306266719e-10,
               0.6221098041892605227126015543416e-13,
              -0.3399615005417721944303330599666e-15,
               0.2683181998482698748957538846666e-17
             ]



-- | Compute the normalized lower incomplete gamma function
-- γ(/s/,/x/). Normalization means that
-- γ(/s/,∞)=1. Uses Algorithm AS 239 by Shea.
incompleteGamma :: Double       -- ^ /s/ ∈ (0,∞)
                -> Double       -- ^ /x/ ∈ (0,∞)
                -> Double
incompleteGamma p x
    | isNaN p || isNaN x = m_NaN
    | x < 0 || p <= 0    = m_pos_inf
    | x == 0             = 0
    -- For very large `p' normal approximation gives <1e-10 error
    | p >= 2e5           = norm (3 * sqrt p * ((x/p) ** (1/3) + 1/(9*p) - 1))
    | p >= 500           = approx
    -- Dubious approximation
    | x >= 1e8           = 1
    | x <= 1 || x < p    = let a = p * log x - x - logGamma (p + 1)
                               g = a + log (pearson p 1 1)
                           in if g > limit then exp g else 0
    | otherwise          = let g = p * log x - x - logGamma p + log cf
                           in if g > limit then 1 - exp g else 1
  where
    -- CDF for standard normal distributions
    norm a = 0.5 * erfc (- a / m_sqrt_2)
    -- For large values of `p' we use 18-point Gauss-Legendre
    -- integration.
    approx
      | ans > 0   = 1 - ans
      | otherwise = -ans
      where
        -- Set upper limit for integration
        xu | x > p1    =         (p1 + 11.5*sqrtP1) `max` (x + 6*sqrtP1)
           | otherwise = max 0 $ (p1 -  7.5*sqrtP1) `min` (x - 5*sqrtP1)
        s = U.sum $ U.zipWith go coefY coefW
        go y w = let t = x + (xu - x)*y
                 in w * exp( -(t-p1) + p1*(log t - lnP1) )
        ans = s * (xu - x) * exp( p1 * (lnP1 - 1) - logGamma p)
        --
        p1     = p - 1
        lnP1   = log  p1
        sqrtP1 = sqrt p1
    --
    pearson !a !c !g
        | c' <= tolerance = g'
        | otherwise       = pearson a' c' g'
        where a' = a + 1
              c' = c * x / a'
              g' = g + c'
    cf = let a = 1 - p
             b = a + x + 1
             p3 = x + 1
             p4 = x * b
         in contFrac a b 0 1 x p3 p4 (p3/p4)
    contFrac !a !b !c !p1 !p2 !p3 !p4 !g
        | abs (g - rn) <= min tolerance (tolerance * rn) = g
        | otherwise = contFrac a' b' c' (f p3) (f p4) (f p5) (f p6) rn
        where a' = a + 1
              b' = b + 2
              c' = c + 1
              an = a' * c'
              p5 = b' * p3 - an * p1
              p6 = b' * p4 - an * p2
              rn = p5 / p6
              f n | abs p5 > overflow = n / overflow
                  | otherwise         = n
    limit     = -88
    tolerance = 1e-14
    overflow  = 1e37



-- Adapted from Numerical Recipes §6.2.1

-- | Inverse incomplete gamma function. It's approximately inverse of
--   'incompleteGamma' for the same /s/. So following equality
--   approximately holds:
--
-- > invIncompleteGamma s . incompleteGamma s = id
invIncompleteGamma :: Double    -- ^ /s/ ∈ (0,∞)
                   -> Double    -- ^ /p/ ∈ [0,1]
                   -> Double
invIncompleteGamma a p
  | a <= 0         =
      modErr $ printf "invIncompleteGamma: a must be positive. a=%g p=%g" a p
  | p < 0 || p > 1 =
      modErr $ printf "invIncompleteGamma: p must be in [0,1] range. a=%g p=%g" a p
  | p == 0         = 0
  | p == 1         = 1 / 0
  | otherwise      = loop 0 guess
  where
    -- Solve equation γ(a,x) = p using Halley method
    loop :: Int -> Double -> Double
    loop i x
      | i >= 12           = x'
      -- For small s derivative becomes approximately 1/x*exp(-x) and
      -- skyrockets for small x. If it happens correct answer is 0.
      | isInfinite f'     = 0
      | abs dx < eps * x' = x'
      | otherwise         = loop (i + 1) x'
      where
        -- Value of γ(a,x) - p
        f    = incompleteGamma a x - p
        -- dγ(a,x)/dx
        f'   | a > 1     = afac * exp( -(x - a1) + a1 * (log x - lna1))
             | otherwise = exp( -x + a1 * log x - gln)
        u    = f / f'
        -- Halley correction to Newton-Rapson step
        corr = u * (a1 / x - 1)
        dx   = u / (1 - 0.5 * min 1.0 corr)
        -- New approximation to x
        x'   | x < dx    = 0.5 * x -- Do not go below 0
             | otherwise = x - dx
    -- Calculate inital guess for root
    guess
      --
      | a > 1   =
         let t  = sqrt $ -2 * log(if p < 0.5 then p else 1 - p)
             x1 = (2.30753 + t * 0.27061) / (1 + t * (0.99229 + t * 0.04481)) - t
             x2 = if p < 0.5 then -x1 else x1
         in max 1e-3 (a * (1 - 1/(9*a) - x2 / (3 * sqrt a)) ** 3)
      -- For a <= 1 use following approximations:
      --   γ(a,1) ≈ 0.253a + 0.12a²
      --
      --   γ(a,x) ≈ γ(a,1)·x^a                               x <  1
      --   γ(a,x) ≈ γ(a,1) + (1 - γ(a,1))(1 - exp(1 - x))    x >= 1
      | otherwise =
         let t = 1 - a * (0.253 + a*0.12)
         in if p < t
            then (p / t) ** (1 / a)
            else 1 - log( 1 - (p-t) / (1-t))
    -- Constants
    a1   = a - 1
    lna1 = log a1
    afac = exp( a1 * (lna1 - 1) - gln )
    gln  = logGamma a
    eps  = 1e-8



----------------------------------------------------------------
-- Beta function
----------------------------------------------------------------

-- | Compute the natural logarithm of the beta function.
logBeta :: Double -> Double -> Double
logBeta a b
    | p < 0     = m_NaN
    | p == 0    = m_pos_inf
    | p >= 10   = log q * (-0.5) + m_ln_sqrt_2_pi + logGammaCorrection p + c +
                  (p - 0.5) * log ppq + q * log1p(-ppq)
    | q >= 10   = logGamma p + c + p - p * log pq + (q - 0.5) * log1p(-ppq)
    | otherwise = logGamma p + logGamma q - logGamma pq
    where
      p   = min a b
      q   = max a b
      ppq = p / pq
      pq  = p + q
      c   = logGammaCorrection q - logGammaCorrection pq

-- | Regularized incomplete beta function. Uses algorithm AS63 by
-- Majumder and Bhattachrjee and quadrature approximation for large
-- /p/ and /q/.
incompleteBeta :: Double -- ^ /p/ > 0
               -> Double -- ^ /q/ > 0
               -> Double -- ^ /x/, must lie in [0,1] range
               -> Double
incompleteBeta p q = incompleteBeta_ (logBeta p q) p q

-- | Regularized incomplete beta function. Same as 'incompleteBeta'
-- but also takes logarithm of beta function as parameter.
incompleteBeta_ :: Double -- ^ logarithm of beta function for given /p/ and /q/
                -> Double -- ^ /p/ > 0
                -> Double -- ^ /q/ > 0
                -> Double -- ^ /x/, must lie in [0,1] range
                -> Double
incompleteBeta_ beta p q x
  | p <= 0 || q <= 0            =
      modErr $ printf "incompleteBeta_: p <= 0 || q <= 0. p=%g q=%g x=%g" p q x
  | x <  0 || x >  1 || isNaN x =
      modErr $ printf "incompletBeta_: x out of [0,1] range. p=%g q=%g x=%g" p q x
  | x == 0 || x == 1            = x
  | p >= (p+q) * x   = incompleteBetaWorker beta p q x
  | otherwise        = 1 - incompleteBetaWorker beta q p (1 - x)


-- Approximation of incomplete beta by quandrature.
--
-- Note that x =< p/(p+q)
incompleteBetaApprox :: Double -> Double -> Double -> Double -> Double
incompleteBetaApprox beta p q x
  | ans > 0   = 1 - ans
  | otherwise = -ans
  where
    -- Constants
    p1    = p - 1
    q1    = q - 1
    mu    = p / (p + q)
    lnmu  = log mu
    lnmuc = log (1 - mu)
    -- Upper limit for integration
    xu = max 0 $ min (mu - 10*t) (x - 5*t)
       where
         t = sqrt $ p*q / ( (p+q) * (p+q) * (p + q + 1) )
    -- Calculate incomplete beta by quadrature
    go y w = let t = x + (xu - x) * y
             in  w * exp( p1 * (log t - lnmu) + q1 * (log(1-t) - lnmuc) )
    s   = U.sum $ U.zipWith go coefY coefW
    ans = s * (xu - x) * exp( p1 * lnmu + q1 * lnmuc - beta )


-- Worker for incomplete beta function. It is separate function to
-- avoid confusion with parameter during parameter swapping
incompleteBetaWorker :: Double -> Double -> Double -> Double -> Double
incompleteBetaWorker beta p q x
  -- For very large p and q this method becomes very slow so another
  -- method is used.
  | p > 3000 && q > 3000 = incompleteBetaApprox beta p q x
  | otherwise            = loop (p+q) (truncate $ q + cx * (p+q)) 1 1 1
  where
    -- Constants
    eps = 1e-15
    cx  = 1 - x
    -- Loop
    loop !psq (ns :: Int) ai term betain
      | done      = betain' * exp( p * log x + (q - 1) * log cx - beta) / p
      | otherwise = loop psq' (ns - 1) (ai + 1) term' betain'
      where
        -- New values
        term'   = term * fact / (p + ai)
        betain' = betain + term'
        fact | ns >  0   = (q - ai) * x/cx
             | ns == 0   = (q - ai) * x
             | otherwise = psq * x
        -- Iterations are complete
        done = db <= eps && db <= eps*betain' where db = abs term'
        psq' = if ns < 0 then psq + 1 else psq



-- | Compute inverse of regularized incomplete beta function. Uses
-- initial approximation from AS109, AS64 and Halley method to solve
-- equation.
invIncompleteBeta :: Double     -- ^ /p/ > 0
                  -> Double     -- ^ /q/ > 0
                  -> Double     -- ^ /a/ ∈ [0,1]
                  -> Double
invIncompleteBeta p q a
  | p <= 0 || q <= 0 =
      modErr $ printf "invIncompleteBeta p <= 0 || q <= 0.  p=%g q=%g a=%g" p q a
  | a <  0 || a >  1 =
      modErr $ printf "invIncompleteBeta x must be in [0,1].  p=%g q=%g a=%g" p q a
  | a == 0 || a == 1 = a
  | a > 0.5          = 1 - invIncompleteBetaWorker (logBeta p q) q p (1 - a)
  | otherwise        =     invIncompleteBetaWorker (logBeta p q) p q  a


invIncompleteBetaWorker :: Double -> Double -> Double -> Double -> Double
-- NOTE: p <= 0.5.
invIncompleteBetaWorker beta a b p = loop (0::Int) guess
  where
    a1 = a - 1
    b1 = b - 1
    -- Solve equation using Halley method
    loop !i !x
      -- We cannot continue at this point so we simply return `x'
      | x == 0 || x == 1             = x
      -- When derivative becomes infinite we cannot continue
      -- iterations. It can only happen in vicinity of 0 or 1. It's
      -- hardly possible to get good answer in such circumstances but
      -- `x' is already reasonable.
      | isInfinite f'                = x
      -- Iterations limit reached. Most of the time solution will
      -- converge to answer because of discreteness of Double. But
      -- solution have good precision already.
      | i >= 10                      = x
      -- Solution converges
      | abs dx <= 16 * m_epsilon * x = x'
      | otherwise                    = loop (i+1) x'
      where
        -- Calculate Halley step.
        f   = incompleteBeta_ beta a b x - p
        f'  = exp $ a1 * log x + b1 * log (1 - x) - beta
        u   = f / f'
        dx  = u / (1 - 0.5 * min 1 (u * (a1 / x - b1 / (1 - x))))
        -- Next approximation. If Halley step leads us out of [0,1]
        -- range we revert to bisection.
        x'  | z < 0     = x / 2
            | z > 1     = (x + 1) / 2
            | otherwise = z
            where z = x - dx
    -- Calculate initial guess. Approximations from AS64, AS109 and
    -- Numerical recipes are used.
    --
    -- Equations are referred to by name of paper and number e.g. [AS64 2]
    -- In AS64 papers equations are not numbered so they are refered
    -- to by number of appearance starting from definition of
    -- incomplete beta.
    guess
      -- In this region we use approximation from AS109 (Carter
      -- approximation). It's reasonably good (2 iterations on
      -- average)
      | a > 1 && b > 1 =
          let r = (y*y - 3) / 6
              s = 1 / (2*a - 1)
              t = 1 / (2*b - 1)
              h = 2 / (s + t)
              w = y * sqrt(h + r) / h - (t - s) * (r + 5/6 - 2 / (3 * h))
          in a / (a + b * exp(2 * w))
      -- Otherwise we revert to approximation from AS64 derived from
      -- [AS64 2] when it's applicable.
      --
      -- It slightly reduces average number of iterations when `a' and
      -- `b' have different magnitudes.
      | chi2 > 0 && ratio > 1 = 1 - 2 / (ratio + 1)
      -- If all else fails we use approximation from "Numerical
      -- Recipes". It's very similar to approximations [AS64 4,5] but
      -- it never goes out of [0,1] interval.
      | otherwise = case () of
          _| p < t / w  -> (a * p * w) ** (1/a)
           | otherwise  -> 1 - (b * (1 - p) * w) ** (1/b)
           where
             lna = log $ a / (a+b)
             lnb = log $ b / (a+b)
             t   = exp( a * lna ) / a
             u   = exp( b * lnb ) / b
             w   = t + u
      where
        -- Formula [2]
        ratio = (4*a + 2*b - 2) / chi2
        -- Quantile of chi-squared distribution. Formula [3].
        chi2 = 2 * b * (1 - t + y * sqrt t) ** 3
          where
            t   = 1 / (9 * b)
        -- `y' is Hasting's approximation of p'th quantile of standard
        -- normal distribution.
        y   = r - ( 2.30753 + 0.27061 * r )
                  / ( 1.0 + ( 0.99229 + 0.04481 * r ) * r )
          where
            r = sqrt $ - 2 * log p




----------------------------------------------------------------
-- Logarithm
----------------------------------------------------------------

-- | Compute the natural logarithm of 1 + @x@.  This is accurate even
-- for values of @x@ near zero, where use of @log(1+x)@ would lose
-- precision.
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


-- | /O(log n)/ Compute the logarithm in base 2 of the given value.
log2 :: Int -> Int
log2 v0
    | v0 <= 0   = modErr $ "log2: negative input, got " ++ show v0
    | otherwise = go 5 0 v0
  where
    go !i !r !v | i == -1        = r
                | v .&. b i /= 0 = let si = U.unsafeIndex sv i
                                   in go (i-1) (r .|. si) (v `shiftR` si)
                | otherwise      = go (i-1) r v
    b = U.unsafeIndex bv
    !bv = U.fromList [0x2, 0xc, 0xf0, 0xff00, 0xffff0000, 0xffffffff00000000]
    !sv = U.fromList [1,2,4,8,16,32]


----------------------------------------------------------------
-- Factorial
----------------------------------------------------------------

-- | Compute the factorial function /n/!.  Returns +∞ if the
-- input is above 170 (above which the result cannot be represented by
-- a 64-bit 'Double').
factorial :: Int -> Double
factorial n
    | n < 0     = error "Numeric.SpecFunctions.factorial: negative input"
    | n <= 1    = 1
    | n <= 170  = U.product $ U.map fromIntegral $ U.enumFromTo 2 n
    | otherwise = m_pos_inf

-- | Compute the natural logarithm of the factorial function.  Gives
-- 16 decimal digits of precision.
logFactorial :: Integral a => a -> Double
logFactorial n
    | n <  0    = error "Numeric.SpecFunctions.logFactorial: negative input"
    | n <= 14   = log $ factorial $ fromIntegral n
    | otherwise = (x - 0.5) * log x - x + 9.1893853320467e-1 + z / x
    where x = fromIntegral n + 1
          y = 1 / (x * x)
          z = ((-(5.95238095238e-4 * y) + 7.936500793651e-4) * y -
               2.7777777777778e-3) * y + 8.3333333333333e-2
{-# SPECIALIZE logFactorial :: Int -> Double #-}

-- | Calculate the error term of the Stirling approximation.  This is
-- only defined for non-negative values.
--
-- > stirlingError @n@ = @log(n!) - log(sqrt(2*pi*n)*(n/e)^n)
stirlingError :: Double -> Double
stirlingError n
  | n <= 15.0   = case properFraction (n+n) of
                    (i,0) -> sfe `U.unsafeIndex` i
                    _     -> logGamma (n+1.0) - (n+0.5) * log n + n -
                             m_ln_sqrt_2_pi
  | n > 500     = evaluateOddPolynomialL (1/n) [s0,-s1]
  | n > 80      = evaluateOddPolynomialL (1/n) [s0,-s1,s2]
  | n > 35      = evaluateOddPolynomialL (1/n) [s0,-s1,s2,-s3]
  | otherwise   = evaluateOddPolynomialL (1/n) [s0,-s1,s2,-s3,s4]
  where
    s0 = 0.083333333333333333333        -- 1/12
    s1 = 0.00277777777777777777778      -- 1/360
    s2 = 0.00079365079365079365079365   -- 1/1260
    s3 = 0.000595238095238095238095238  -- 1/1680
    s4 = 0.0008417508417508417508417508 -- 1/1188
    sfe = U.fromList [ 0.0,
                0.1534264097200273452913848,   0.0810614667953272582196702,
                0.0548141210519176538961390,   0.0413406959554092940938221,
                0.03316287351993628748511048,  0.02767792568499833914878929,
                0.02374616365629749597132920,  0.02079067210376509311152277,
                0.01848845053267318523077934,  0.01664469118982119216319487,
                0.01513497322191737887351255,  0.01387612882307074799874573,
                0.01281046524292022692424986,  0.01189670994589177009505572,
                0.01110455975820691732662991,  0.010411265261972096497478567,
                0.009799416126158803298389475, 0.009255462182712732917728637,
                0.008768700134139385462952823, 0.008330563433362871256469318,
                0.007934114564314020547248100, 0.007573675487951840794972024,
                0.007244554301320383179543912, 0.006942840107209529865664152,
                0.006665247032707682442354394, 0.006408994188004207068439631,
                0.006171712263039457647532867, 0.005951370112758847735624416,
                0.005746216513010115682023589, 0.005554733551962801371038690 ]


----------------------------------------------------------------
-- Combinatorics
----------------------------------------------------------------

-- | Quickly compute the natural logarithm of /n/ @`choose`@ /k/, with
-- no checking.
logChooseFast :: Double -> Double -> Double
logChooseFast n k = -log (n + 1) - logBeta (n - k + 1) (k + 1)

-- | Compute the binomial coefficient /n/ @\``choose`\`@ /k/. For
-- values of /k/ > 30, this uses an approximation for performance
-- reasons.  The approximation is accurate to 12 decimal places in the
-- worst case
--
-- Example:
--
-- > 7 `choose` 3 == 35
choose :: Int -> Int -> Double
n `choose` k
    | k  > n         = 0
    | k' < 50        = U.foldl' go 1 . U.enumFromTo 1 $ k'
    | approx < max64 = fromIntegral . round64 $ approx
    | otherwise      = approx
  where
    k'             = min k (n-k)
    approx         = exp $ logChooseFast (fromIntegral n) (fromIntegral k')
                  -- Less numerically stable:
                  -- exp $ lg (n+1) - lg (k+1) - lg (n-k+1)
                  --   where lg = logGamma . fromIntegral
    go a i         = a * (nk + j) / j
        where j    = fromIntegral i :: Double
    nk             = fromIntegral (n - k')
    max64          = fromIntegral (maxBound :: Int64)
    round64 x      = round x :: Int64

-- | Compute ψ0(/x/), the first logarithmic derivative of the gamma
-- function. Uses Algorithm AS 103 by Bernardo, based on Minka's C
-- implementation.
digamma :: Double -> Double
digamma x
    | isNaN x || isInfinite x                  = m_NaN
    -- FIXME:
    --   This is ugly. We are testing here that number is in fact
    --   integer. It's somewhat tricky question to answer. When ε for
    --   given number becomes 1 or greater every number is represents
    --   an integer. We also must make sure that excess precision
    --   won't bite us.
    | x <= 0 && fromIntegral (truncate x :: Int64) == x = m_neg_inf
    -- Jeffery's reflection formula
    | x < 0     = digamma (1 - x) + pi / tan (negate pi * x)
    | x <= 1e-6 = - γ - 1/x + trigamma1 * x
    | x' < c    = r
    -- De Moivre's expansion
    | otherwise = let s = 1/x'
                  in  evaluateEvenPolynomialL s
                        [   r + log x' - 0.5 * s
                        , - 1/12
                        ,   1/120
                        , - 1/252
                        ,   1/240
                        , - 1/132
                        ,  391/32760
                        ]
  where
    γ  = m_eulerMascheroni
    c  = 12
    -- Reduce to digamma (x + n) where (x + n) >= c
    (r, x') = reduce 0 x
      where
        reduce !s y
          | y < c     = reduce (s - 1 / y) (y + 1)
          | otherwise = (s, y)



----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

-- Coefficients for 18-point Gauss-Legendre integration. They are
-- used in implementation of incomplete gamma and beta functions.
coefW,coefY :: U.Vector Double
coefW = U.fromList [ 0.0055657196642445571, 0.012915947284065419, 0.020181515297735382
                   , 0.027298621498568734,  0.034213810770299537, 0.040875750923643261
                   , 0.047235083490265582,  0.053244713977759692, 0.058860144245324798
                   , 0.064039797355015485,  0.068745323835736408, 0.072941885005653087
                   , 0.076598410645870640,  0.079687828912071670, 0.082187266704339706
                   , 0.084078218979661945,  0.085346685739338721, 0.085983275670394821
                   ]
coefY = U.fromList [ 0.0021695375159141994, 0.011413521097787704, 0.027972308950302116
                   , 0.051727015600492421,  0.082502225484340941, 0.12007019910960293
                   , 0.16415283300752470,   0.21442376986779355,  0.27051082840644336
                   , 0.33199876341447887,   0.39843234186401943,  0.46931971407375483
                   , 0.54413605556657973,   0.62232745288031077,  0.70331500465597174
                   , 0.78649910768313447,   0.87126389619061517,  0.95698180152629142
                   ]
{-# NOINLINE coefW #-}
{-# NOINLINE coefY #-}

trigamma1 :: Double
trigamma1 = 1.6449340668482264365 -- pi**2 / 6

modErr :: String -> a
modErr msg = error $ "Numeric.SpecFunctions." ++ msg



-- $references
--
-- * Bernardo, J. (1976) Algorithm AS 103: Psi (digamma)
--   function. /Journal of the Royal Statistical Society. Series C
--   (Applied Statistics)/ 25(3):315-317.
--   <http://www.jstor.org/stable/2347257>
--
-- * Cran, G.W., Martin, K.J., Thomas, G.E. (1977) Remark AS R19
--   and Algorithm AS 109: A Remark on Algorithms: AS 63: The
--   Incomplete Beta Integral AS 64: Inverse of the Incomplete Beta
--   Function Ratio. /Journal of the Royal Statistical Society. Series
--   C (Applied Statistics)/ Vol. 26, No. 1 (1977), pp. 111-114
--   <http://www.jstor.org/pss/2346887>
--
-- * Lanczos, C. (1964) A precision approximation of the gamma
--   function.  /SIAM Journal on Numerical Analysis B/
--   1:86&#8211;96. <http://www.jstor.org/stable/2949767>
--
-- * Loader, C. (2000) Fast and Accurate Computation of Binomial
--   Probabilities. <http://projects.scipy.org/scipy/raw-attachment/ticket/620/loader2000Fast.pdf>
--
-- * Macleod, A.J. (1989) Algorithm AS 245: A robust and reliable
--   algorithm for the logarithm of the gamma function.
--   /Journal of the Royal Statistical Society, Series C (Applied Statistics)/
--   38(2):397&#8211;402. <http://www.jstor.org/stable/2348078>
--
-- * Majumder, K.L., Bhattacharjee, G.P. (1973) Algorithm AS 63: The
--   Incomplete Beta Integral. /Journal of the Royal Statistical
--   Society. Series C (Applied Statistics)/ Vol. 22, No. 3 (1973),
--   pp. 409-411. <http://www.jstor.org/pss/2346797>
--
-- * Majumder, K.L., Bhattacharjee, G.P. (1973) Algorithm AS 64:
--   Inverse of the Incomplete Beta Function Ratio. /Journal of the
--   Royal Statistical Society. Series C (Applied Statistics)/
--   Vol. 22, No. 3 (1973), pp. 411-414
--   <http://www.jstor.org/pss/2346798>
--
-- * Shea, B. (1988) Algorithm AS 239: Chi-squared and incomplete
--   gamma integral. /Applied Statistics/
--   37(3):466&#8211;473. <http://www.jstor.org/stable/2347328>
