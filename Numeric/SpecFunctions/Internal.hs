{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module    : Numeric.SpecFunctions.Internal
-- Copyright : (c) 2009, 2011, 2012 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Internal module with implementation of special functions.
module Numeric.SpecFunctions.Internal
    ( module Numeric.SpecFunctions.Internal
    , Compat.log1p
    , Compat.expm1
    ) where

import Control.Applicative
import Data.Bits          ((.&.), (.|.), shiftR)
import Data.Int           (Int64)
import Data.Word          (Word)
import Data.Default.Class
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed   ((!))
import Text.Printf

import Numeric.Polynomial.Chebyshev    (chebyshevBroucke)
import Numeric.Polynomial              (evaluatePolynomial, evaluatePolynomialL, evaluateEvenPolynomialL
                                       ,evaluateOddPolynomialL)
import Numeric.RootFinding             (Root(..), newtonRaphson, NewtonParam(..), Tolerance(..))
import Numeric.Series
import Numeric.MathFunctions.Constants
import Numeric.SpecFunctions.Compat (log1p)
import qualified Numeric.SpecFunctions.Compat as Compat

----------------------------------------------------------------
-- Error function
----------------------------------------------------------------

-- | Error function.
--
-- \[
-- \operatorname{erf}(x) = \frac{2}{\sqrt{\pi}} \int_{0}^{x} \exp(-t^2) dt
-- \]
--
-- Function limits are:
--
-- \[
-- \begin{aligned}
--  &\operatorname{erf}(-\infty) &=& -1 \\
--  &\operatorname{erf}(0)       &=& \phantom{-}\,0 \\
--  &\operatorname{erf}(+\infty) &=& \phantom{-}\,1 \\
-- \end{aligned}
-- \]
erf :: Double -> Double
erf = Compat.erf
{-# INLINE erf #-}

-- | Complementary error function.
--
-- \[
-- \operatorname{erfc}(x) = 1 - \operatorname{erf}(x)
-- \]
--
-- Function limits are:
--
-- \[
-- \begin{aligned}
--  &\operatorname{erf}(-\infty) &=&\, 2 \\
--  &\operatorname{erf}(0)       &=&\, 1 \\
--  &\operatorname{erf}(+\infty) &=&\, 0 \\
-- \end{aligned}
-- \]
erfc :: Double -> Double
erfc = Compat.erfc
{-# INLINE erfc #-}

-- | Inverse of 'erf'.
invErf :: Double -- ^ /p/ ∈ [-1,1]
       -> Double
invErf p
  | p ==  1         = m_pos_inf
  | p == -1         = m_neg_inf
  | p < 1 && p > -1 = if p > 0 then r else -r
  | otherwise       = error "invErf: p must in [-1,1] range"
  where
    -- We solve equation same as in invErfc. We're able to ruse same
    -- Halley step by solving equation:
    --   > pp - erf x = 0
    -- instead of
    --   > erf x - pp = 0
    pp     = abs p
    r      = step $ step $ guessInvErfc $ 1 - pp
    step x = invErfcHalleyStep (pp - erf x) x

-- | Inverse of 'erfc'.
invErfc :: Double -- ^ /p/ ∈ [0,2]
        -> Double
invErfc p
  | p == 2        = m_neg_inf
  | p == 0        = m_pos_inf
  | p >0 && p < 2 = if p <= 1 then r else -r
  | otherwise     = modErr $ "invErfc: p must be in [0,2] got " ++ show p
  where
    pp | p <= 1    = p
       | otherwise = 2 - p
    -- We perform 2 Halley steps in order to get to solution
    r      = step $ step $ guessInvErfc pp
    step x = invErfcHalleyStep (erfc x - pp) x

-- Initial guess for invErfc & invErf
guessInvErfc :: Double -> Double
guessInvErfc p
  = -0.70711 * ((2.30753 + t * 0.27061) / (1 + t * (0.99229 + t * 0.04481)) - t)
  where
    t = sqrt $ -2 * log( 0.5 * p)

-- Halley step for solving invErfc
invErfcHalleyStep :: Double -> Double -> Double
invErfcHalleyStep err x
  = x + err / (1.12837916709551257 * exp(-x * x) - x * err)

----------------------------------------------------------------
-- Gamma function
----------------------------------------------------------------

data L = L {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Compute the logarithm of the gamma function, Γ(/x/).
--
-- \[
-- \Gamma(x) = \int_0^{\infty}t^{x-1}e^{-t}\,dt = (x - 1)!
-- \]
--
-- This implementation uses Lanczos approximation. It gives 14 or more
-- significant decimal digits, except around /x/ = 1 and /x/ = 2,
-- where the function goes to zero.
--
-- Returns &#8734; if the input is outside of the range (0 < /x/
-- &#8804; 1e305).
logGamma :: Double -> Double
logGamma z
  | z <= 0    = m_pos_inf
  -- For very small values z we can just use Laurent expansion
  | z < m_sqrt_eps = log (1/z - m_eulerMascheroni)
  -- For z<1 we use recurrence. Γ(z+1) = z·Γ(z) Note that in order to
  -- avoid precision loss we have to compute parameter to
  -- approximations here:
  --
  -- > (z + 1) - 1 = z
  -- > (z + 1) - 2 = z - 1
  --
  -- Simple passing (z + 1) to piecewise approxiations and computing
  -- difference leads to bad loss of precision near 1.
  -- This is reason lgamma1_15 & lgamma15_2 have three parameters
  | z < 0.5   = lgamma1_15 z (z - 1) - log z
  | z < 1     = lgamma15_2 z (z - 1) - log z
  -- Piecewise polynomial approximations
  | z <= 1.5  = lgamma1_15 (z - 1) (z - 2)
  | z < 2     = lgamma15_2 (z - 1) (z - 2)
  | z < 15    = lgammaSmall z
  -- Otherwise we switch to Lanczos approximation
  | otherwise = lanczosApprox z


-- | Synonym for 'logGamma'. Retained for compatibility
logGammaL :: Double -> Double
logGammaL = logGamma
{-# DEPRECATED logGammaL "Use logGamma instead" #-}



-- Polynomial expansion used in interval (1,1.5]
--
-- > logΓ(z) = (z-1)(z-2)(Y + R(z-1))
lgamma1_15 :: Double -> Double -> Double
lgamma1_15 zm1 zm2
   = r * y + r * ( evaluatePolynomial zm1 tableLogGamma_1_15P
                 / evaluatePolynomial zm1 tableLogGamma_1_15Q
                 )
   where
     r = zm1 * zm2
     y = 0.52815341949462890625

tableLogGamma_1_15P,tableLogGamma_1_15Q :: U.Vector Double
tableLogGamma_1_15P = U.fromList
  [  0.490622454069039543534e-1
  , -0.969117530159521214579e-1
  , -0.414983358359495381969e0
  , -0.406567124211938417342e0
  , -0.158413586390692192217e0
  , -0.240149820648571559892e-1
  , -0.100346687696279557415e-2
  ]
{-# NOINLINE tableLogGamma_1_15P #-}
tableLogGamma_1_15Q = U.fromList
  [ 1
  , 0.302349829846463038743e1
  , 0.348739585360723852576e1
  , 0.191415588274426679201e1
  , 0.507137738614363510846e0
  , 0.577039722690451849648e-1
  , 0.195768102601107189171e-2
  ]
{-# NOINLINE tableLogGamma_1_15Q #-}



-- Polynomial expansion used in interval (1.5,2)
--
-- > logΓ(z) = (2-z)(1-z)(Y + R(2-z))
lgamma15_2 :: Double -> Double -> Double
lgamma15_2 zm1 zm2
   = r * y + r * ( evaluatePolynomial (-zm2) tableLogGamma_15_2P
                 / evaluatePolynomial (-zm2) tableLogGamma_15_2Q
                 )
   where
     r = zm1 * zm2
     y = 0.452017307281494140625

tableLogGamma_15_2P,tableLogGamma_15_2Q :: U.Vector Double
tableLogGamma_15_2P = U.fromList
  [ -0.292329721830270012337e-1
  ,  0.144216267757192309184e0
  , -0.142440390738631274135e0
  ,  0.542809694055053558157e-1
  , -0.850535976868336437746e-2
  ,  0.431171342679297331241e-3
  ]
{-# NOINLINE tableLogGamma_15_2P #-}
tableLogGamma_15_2Q = U.fromList
  [  1
  , -0.150169356054485044494e1
  ,  0.846973248876495016101e0
  , -0.220095151814995745555e0
  ,  0.25582797155975869989e-1
  , -0.100666795539143372762e-2
  , -0.827193521891290553639e-6
  ]
{-# NOINLINE tableLogGamma_15_2Q #-}



-- Polynomial expansion used in interval (2,3)
--
-- > logΓ(z) = (z - 2)(z + 1)(Y + R(z-2))
lgamma2_3 :: Double -> Double
lgamma2_3 z
  = r * y + r * ( evaluatePolynomial zm2 tableLogGamma_2_3P
                / evaluatePolynomial zm2 tableLogGamma_2_3Q
                )
  where
    r   = zm2 * (z + 1)
    zm2 = z - 2
    y   = 0.158963680267333984375e0


tableLogGamma_2_3P,tableLogGamma_2_3Q :: U.Vector Double
tableLogGamma_2_3P = U.fromList
  [ -0.180355685678449379109e-1
  ,  0.25126649619989678683e-1
  ,  0.494103151567532234274e-1
  ,  0.172491608709613993966e-1
  , -0.259453563205438108893e-3
  , -0.541009869215204396339e-3
  , -0.324588649825948492091e-4
  ]
{-# NOINLINE tableLogGamma_2_3P #-}
tableLogGamma_2_3Q = U.fromList
  [  1
  ,  0.196202987197795200688e1
  ,  0.148019669424231326694e1
  ,  0.541391432071720958364e0
  ,  0.988504251128010129477e-1
  ,  0.82130967464889339326e-2
  ,  0.224936291922115757597e-3
  , -0.223352763208617092964e-6
  ]
{-# NOINLINE tableLogGamma_2_3Q #-}



-- For small z we can just use Gamma function recurrence and reduce
-- problem to interval [2,3] and use polynomial approximation
-- there. Surpringly it gives very good precision
lgammaSmall :: Double -> Double
lgammaSmall = go 0
  where
    go acc z | z < 3     = acc + lgamma2_3 z
             | otherwise = go (acc + log zm1) zm1
             where
               zm1 = z - 1


-- Lanczos approximation for gamma function.
--
-- > Γ(z) = sqrt(2π)(z + g - 0.5)^(z - 0.5)·exp{-(z + g - 0.5)}·A_g(z)
--
-- Coeffients are taken from boost. Constants are absorbed into
-- polynomial's coefficients.
lanczosApprox :: Double -> Double
lanczosApprox z
  = (log (z + g - 0.5) - 1) * (z - 0.5)
  + log (evalRatio tableLanczos z)
  where
    g = 6.024680040776729583740234375

tableLanczos :: U.Vector (Double,Double)
{-# NOINLINE tableLanczos #-}
tableLanczos = U.fromList
  [ (56906521.91347156388090791033559122686859    , 0)
  , (103794043.1163445451906271053616070238554    , 39916800)
  , (86363131.28813859145546927288977868422342    , 120543840)
  , (43338889.32467613834773723740590533316085    , 150917976)
  , (14605578.08768506808414169982791359218571    , 105258076)
  , (3481712.15498064590882071018964774556468     , 45995730)
  , (601859.6171681098786670226533699352302507    , 13339535)
  , (75999.29304014542649875303443598909137092    , 2637558)
  , (6955.999602515376140356310115515198987526    , 357423)
  , (449.9445569063168119446858607650988409623    , 32670)
  , (19.51992788247617482847860966235652136208    , 1925)
  , (0.5098416655656676188125178644804694509993   , 66)
  , (0.006061842346248906525783753964555936883222 , 1)
  ]

-- Evaluate rational function. Polynomials in both numerator and
-- denominator must have same order. Function seems to be too specific
-- so it's not exposed
--
-- Special care taken in order to avoid overflow for large values of x
evalRatio :: U.Vector (Double,Double) -> Double -> Double
evalRatio coef x
  | x > 1     = fini $ U.foldl' stepL (L 0 0) coef
  | otherwise = fini $ U.foldr' stepR (L 0 0) coef
  where
    fini (L num den) = num / den
    stepR (a,b) (L num den) = L (num * x  + a) (den * x  + b)
    stepL (L num den) (a,b) = L (num * rx + a) (den * rx + b)
    rx = recip x



-- |
-- Compute the log gamma correction factor for Stirling
-- approximation for @x@ &#8805; 10.  This correction factor is
-- suitable for an alternate (but less numerically accurate)
-- definition of 'logGamma':
--
-- \[
-- \log\Gamma(x) = \frac{1}{2}\log(2\pi) + (x-\frac{1}{2})\log x - x + \operatorname{logGammaCorrection}(x)
-- \]
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
-- γ(/z/,/x/). Normalization means that γ(/z/,∞)=1
--
-- \[
-- \gamma(z,x) = \frac{1}{\Gamma(z)}\int_0^{x}t^{z-1}e^{-t}\,dt
-- \]
--
-- Uses Algorithm AS 239 by Shea.
incompleteGamma :: Double       -- ^ /z/ ∈ (0,∞)
                -> Double       -- ^ /x/ ∈ (0,∞)
                -> Double
-- Notation used:
--  + P(a,x) - regularized lower incomplete gamma
--  + Q(a,x) - regularized upper incomplete gamma
incompleteGamma a x
  | a <= 0 || x < 0 = error
     $ "incompleteGamma: Domain error z=" ++ show a ++ " x=" ++ show x
  | x == 0          = 0
  | x == m_pos_inf  = 1
  -- For very small x we use following expansion for P:
  --
  -- See http://functions.wolfram.com/GammaBetaErf/GammaRegularized/06/01/05/01/01/
  | x < sqrt m_epsilon && a > 1
    = x**a / a / exp (logGamma a) * (1 - a*x / (a + 1))
  | x < 0.5 = case () of
    _| (-0.4)/log x < a  -> taylorSeriesP
     | otherwise         -> taylorSeriesComplQ
  | x < 1.1 = case () of
    _| 0.75*x < a        -> taylorSeriesP
     | otherwise         -> taylorSeriesComplQ
  | a > 20 && useTemme    = uniformExpansion
  | x - (1 / (3 * x)) < a = taylorSeriesP
  | otherwise             = contFraction
  where
    mu = (x - a) / a
    useTemme = (a > 200 && 20/a > mu*mu)
            || (abs mu < 0.4)
    -- Gautschi's algorithm.
    --
    -- Evaluate series for P(a,x). See [Temme1994] Eq. 5.5
    --
    -- FIXME: Term `exp (log x * z - x - logGamma (z+1))` doesn't give full precision
    taylorSeriesP
      = sumPowerSeries x (scanSequence (/) 1 $ enumSequenceFrom (a+1))
      * exp (log x * a - x - logGamma (a+1))
    -- Series for 1-Q(a,x). See [Temme1994] Eq. 5.5
    taylorSeriesComplQ
      = sumPowerSeries (-x) (scanSequence (/) 1 (enumSequenceFrom 1) / enumSequenceFrom a)
      * x**a / exp(logGamma a)
    -- Legendre continued fractions
    contFraction = 1 - ( exp ( log x * a - x - logGamma a )
                       / evalContFractionB frac
                       )
      where
        frac = (\k -> (k*(a-k), x - a + 2*k + 1)) <$> enumSequenceFrom 0
    -- Evaluation based on uniform expansions. See [Temme1994] 5.2
    uniformExpansion =
      let -- Coefficients f_m in paper
          fm :: U.Vector Double
          fm = U.fromList [ 1.00000000000000000000e+00
                          ,-3.33333333333333370341e-01
                          , 8.33333333333333287074e-02
                          ,-1.48148148148148153802e-02
                          , 1.15740740740740734316e-03
                          , 3.52733686067019369930e-04
                          ,-1.78755144032921825352e-04
                          , 3.91926317852243766954e-05
                          ,-2.18544851067999240532e-06
                          ,-1.85406221071515996597e-06
                          , 8.29671134095308545622e-07
                          ,-1.76659527368260808474e-07
                          , 6.70785354340149841119e-09
                          , 1.02618097842403069078e-08
                          ,-4.38203601845335376897e-09
                          , 9.14769958223679020897e-10
                          ,-2.55141939949462514346e-11
                          ,-5.83077213255042560744e-11
                          , 2.43619480206674150369e-11
                          ,-5.02766928011417632057e-12
                          , 1.10043920319561347525e-13
                          , 3.37176326240098513631e-13
                          ]
          y   = - log1pmx mu
          eta = sqrt (2 * y) * signum mu
          -- Evaluate S_α (Eq. 5.9)
          loop !_  !_  u 0 = u
          loop bm1 bm0 u i = let t  = (fm ! i) + (fromIntegral i + 1)*bm1 / a
                                 u' = eta * u + t
                             in  loop bm0 t u' (i-1)
          s_a = let n = U.length fm
                in  loop (fm ! (n-1)) (fm ! (n-2)) 0 (n-3)
                  / exp (logGammaCorrection a)
      in 1/2 * erfc(-eta*sqrt(a/2)) - exp(-(a*y)) / sqrt (2*pi*a) * s_a



-- Adapted from Numerical Recipes §6.2.1

-- | Inverse incomplete gamma function. It's approximately inverse of
--   'incompleteGamma' for the same /z/. So following equality
--   approximately holds:
--
-- > invIncompleteGamma z . incompleteGamma z ≈ id
invIncompleteGamma :: Double    -- ^ /z/ ∈ (0,∞)
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
--
-- \[
-- B(a,b) = \int_0^1 t^{a-1}(1-t)^{b-1}\,dt = \frac{\Gamma(a)\Gamma(b)}{\Gamma(a+b)}
-- \]
logBeta
  :: Double                     -- ^ /a/ > 0
  -> Double                     -- ^ /b/ > 0
  -> Double
logBeta a b
  | p < 0     = m_NaN
  | p == 0    = m_pos_inf
  | p >= 10   = allStirling
  | q >= 10   = twoStirling
  -- This order of summands marginally improves precision
  | otherwise = logGamma p + (logGamma q - logGamma pq)
  where
    p   = min a b
    q   = max a b
    ppq = p / pq
    pq  = p + q
    -- When both parameters are large than 10 we can use Stirling
    -- approximation with correction. It's more precise than sum of
    -- logarithms of gamma functions
    allStirling
      = log q * (-0.5)
      + m_ln_sqrt_2_pi
      + logGammaCorrection p
      + (logGammaCorrection q - logGammaCorrection pq)
      + (p - 0.5) * log ppq
      + q * log1p(-ppq)
    -- Otherwise only two of three gamma functions use Stirling
    -- approximation
    twoStirling
      = logGamma p
      + (logGammaCorrection q - logGammaCorrection pq)
      + p
      - p * log pq
      + (q - 0.5) * log1p(-ppq)


-- | Regularized incomplete beta function.
--
-- \[
-- I(x;a,b) = \frac{1}{B(a,b)} \int_0^x t^{a-1}(1-t)^{b-1}\,dt
-- \]
--
-- Uses algorithm AS63 by Majumder and Bhattachrjee and quadrature
-- approximation for large /p/ and /q/.
incompleteBeta :: Double -- ^ /a/ > 0
               -> Double -- ^ /b/ > 0
               -> Double -- ^ /x/, must lie in [0,1] range
               -> Double
incompleteBeta p q = incompleteBeta_ (logBeta p q) p q

-- | Regularized incomplete beta function. Same as 'incompleteBeta'
-- but also takes logarithm of beta function as parameter.
incompleteBeta_ :: Double -- ^ logarithm of beta function for given /p/ and /q/
                -> Double -- ^ /a/ > 0
                -> Double -- ^ /b/ > 0
                -> Double -- ^ /x/, must lie in [0,1] range
                -> Double
incompleteBeta_ beta p q x
  | p <= 0 || q <= 0            =
      modErr $ printf "incompleteBeta_: p <= 0 || q <= 0. p=%g q=%g x=%g" p q x
  | x <  0 || x >  1 || isNaN x =
      modErr $ printf "incompleteBeta_: x out of [0,1] range. p=%g q=%g x=%g" p q x
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
    lnmu  = log     mu
    lnmuc = log1p (-mu)
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
    -- Common multiplies for expansion. Accurate calculation is a bit
    -- tricky. Performing calculation in log-domain leads to slight
    -- loss of precision for small x, while using ** prone to
    -- underflows.
    --
    -- If either beta function of x**p·(1-x)**(q-1) underflows we
    -- switch to log domain. It could waste work but there's no easy
    -- switch criterion.
    factor
      | beta < m_min_log || prod < m_tiny = exp( p * log x + (q - 1) * log cx - beta)
      | otherwise                         = prod / exp beta
      where
        prod =  x**p * cx**(q - 1)
    -- Soper's expansion of incomplete beta function
    loop !psq (ns :: Int) ai term betain
      | done      = betain' * factor / p
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
invIncompleteBeta :: Double     -- ^ /a/ > 0
                  -> Double     -- ^ /b/ > 0
                  -> Double     -- ^ /x/ ∈ [0,1]
                  -> Double
invIncompleteBeta p q a
  | p <= 0 || q <= 0 =
      modErr $ printf "invIncompleteBeta p <= 0 || q <= 0.  p=%g q=%g a=%g" p q a
  | a <  0 || a >  1 =
      modErr $ printf "invIncompleteBeta x must be in [0,1].  p=%g q=%g a=%g" p q a
  | a == 0 || a == 1 = a
  | otherwise        = invIncompleteBetaWorker (logBeta p q) p q  a


invIncompleteBetaWorker :: Double -> Double -> Double -> Double -> Double
invIncompleteBetaWorker beta a b p = loop (0::Int) (invIncBetaGuess beta a b p)
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
        f'  = exp $ a1 * log x + b1 * log1p (-x) - beta
        u   = f / f'
        -- We bound Halley correction to Newton-Raphson to (-1,1) range
        corr | d > 1     = 1
             | d < -1    = -1
             | isNaN d   = 0
             | otherwise = d
          where
            d = u * (a1 / x - b1 / (1 - x))
        dx  = u / (1 - 0.5 * corr)
        -- Next approximation. If Halley step leads us out of [0,1]
        -- range we revert to bisection.
        x'  | z < 0     = x / 2
            | z > 1     = (x + 1) / 2
            | otherwise = z
            where z = x - dx


-- Calculate initial guess for inverse incomplete beta function.
invIncBetaGuess :: Double -> Double -> Double -> Double -> Double
-- Calculate initial guess. for solving equation for inverse incomplete beta.
-- It's really hodgepodge of different approximations accumulated over years.
--
-- Equations are referred to by name of paper and number e.g. [AS64 2]
-- In AS64 papers equations are not numbered so they are refered to by
-- number of appearance starting from definition of incomplete beta.
invIncBetaGuess beta a b p
  -- If both a and b are less than 1 incomplete beta have inflection
  -- point.
  --
  -- > x = (1 - a) / (2 - a - b)
  --
  -- We approximate incomplete beta by neglecting one of factors under
  -- integral and then rescaling result of integration into [0,1]
  -- range.
  | a < 1 && b < 1 =
    let x_infl = (1 - a) / (2 - a - b)
        p_infl = incompleteBeta a b x_infl
        x | p < p_infl = let xg = (a * p     * exp beta) ** (1/a) in xg / (1+xg)
          | otherwise  = let xg = (b * (1-p) * exp beta) ** (1/b) in 1 - xg/(1+xg)
    in x
  -- If both a and b larger or equal that 1 but not too big we use
  -- same approximation as above but calculate it a bit differently
  | a+b <= 6 && a>1 && b>1 =
    let x_infl = (a - 1) / (a + b - 2)
        p_infl = incompleteBeta a b x_infl
        x | p < p_infl = exp ((log(p * a) + beta) / a)
          | otherwise  = 1 - exp((log((1-p) * b) + beta) / b)
    in x
  -- For small a and not too big b we use approximation from boost.
  | b < 5 && a <= 1 =
    let x | p**(1/a) < 0.5 = (p * a * exp beta) ** (1/a)
          | otherwise      = 1 - (1 - p ** (b * exp beta))**(1/b)
    in x
  -- When a>>b and both are large approximation from [Temme1992],
  -- section 4 "the incomplete gamma function case" used. In this
  -- region it greatly improves over other approximation (AS109, AS64,
  -- "Numerical Recipes")
  --
  -- FIXME: It could be used when b>>a too but it require inverse of
  --        upper incomplete gamma to be precise enough. In current
  --        implementation it loses precision in horrible way (40
  --        order of magnitude off for sufficiently small p)
  | a+b > 5 &&  a/b > 4 =
    let -- Calculate initial approximation to eta using eq 4.1
        eta0 = invIncompleteGamma b (1-p) / a
        mu   = b / a            -- Eq. 4.3
        -- A lot of helpers for calculation of
        w    = sqrt(1 + mu)     -- Eq. 4.9
        w_2  = w * w
        w_3  = w_2 * w
        w_4  = w_2 * w_2
        w_5  = w_3 * w_2
        w_6  = w_3 * w_3
        w_7  = w_4 * w_3
        w_8  = w_4 * w_4
        w_9  = w_5 * w_4
        w_10 = w_5 * w_5
        d    = eta0 - mu
        d_2  = d * d
        d_3  = d_2 * d
        d_4  = d_2 * d_2
        w1   = w + 1
        w1_2 = w1 * w1
        w1_3 = w1 * w1_2
        w1_4 = w1_2 * w1_2
        -- Evaluation of eq 4.10
        e1 = (w + 2) * (w - 1) / (3 * w)
           + (w_3 + 9 * w_2 + 21 * w + 5) * d
             / (36 * w_2 * w1)
           - (w_4 - 13 * w_3 + 69 * w_2 + 167 * w + 46) * d_2
             / (1620 * w1_2 * w_3)
           - (7 * w_5 + 21 * w_4 + 70 * w_3 + 26 * w_2 - 93 * w - 31) * d_3
             / (6480 * w1_3 * w_4)
           - (75 * w_6 + 202 * w_5 + 188 * w_4 - 888 * w_3 - 1345 * w_2 + 118 * w + 138) * d_4
             / (272160 * w1_4 * w_5)
        e2 = (28 * w_4 + 131 * w_3 + 402 * w_2 + 581 * w + 208) * (w - 1)
             / (1620 * w1 * w_3)
           - (35 * w_6 - 154 * w_5 - 623 * w_4 - 1636 * w_3 - 3983 * w_2 - 3514 * w - 925) * d
             / (12960 * w1_2 * w_4)
           - ( 2132 * w_7 + 7915 * w_6 + 16821 * w_5 + 35066 * w_4 + 87490 * w_3
             + 141183 * w_2 + 95993 * w + 21640
             ) * d_2
             / (816480 * w_5 * w1_3)
           - ( 11053 * w_8 + 53308 * w_7 + 117010 * w_6 + 163924 * w_5 + 116188 * w_4
             - 258428 * w_3 - 677042 * w_2 - 481940 * w - 105497
             ) * d_3
             / (14696640 * w1_4 * w_6)
        e3 = -( (3592 * w_7 + 8375 * w_6 - 1323 * w_5 - 29198 * w_4 - 89578 * w_3
                - 154413 * w_2 - 116063 * w - 29632
                ) * (w - 1)
              )
              / (816480 * w_5 * w1_2)
           - ( 442043 * w_9 + 2054169 * w_8 + 3803094 * w_7 + 3470754 * w_6 + 2141568 * w_5
             - 2393568 * w_4 - 19904934 * w_3 - 34714674 * w_2 - 23128299 * w - 5253353
             ) * d
             / (146966400 * w_6 * w1_3)
           - ( 116932 * w_10 + 819281 * w_9 + 2378172 * w_8 + 4341330 * w_7 + 6806004 * w_6
             + 10622748 * w_5 + 18739500 * w_4 + 30651894 * w_3 + 30869976 * w_2
             + 15431867 * w + 2919016
             ) * d_2
             / (146966400 * w1_4 * w_7)
        eta = evaluatePolynomialL (1/a) [eta0, e1, e2, e3]
        -- Now we solve eq 4.2 to recover x using Newton iterations
        u       = eta - mu * log eta + (1 + mu) * log(1 + mu) - mu
        cross   = 1 / (1 + mu);
        lower   = if eta < mu then cross else 0
        upper   = if eta < mu then 1     else cross
        x_guess = (lower + upper) / 2
        func x  = ( u + log x + mu*log(1 - x)
                  , 1/x - mu/(1-x)
                  )
        Root x0 = newtonRaphson def{newtonTol=RelTol 1e-8} (lower, x_guess, upper) func
    in x0
  -- For large a and b approximation from AS109 (Carter
  -- approximation). It's reasonably good in this region
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
    -- Formula [AS64 2]
    ratio = (4*a + 2*b - 2) / chi2
    -- Quantile of chi-squared distribution. Formula [AS64 3].
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
-- Sinc function
----------------------------------------------------------------

-- | Compute sinc function @sin(x)\/x@
sinc :: Double -> Double
sinc x
  | ax < eps_0 = 1
  | ax < eps_2 = 1 - x2/6
  | ax < eps_4 = 1 - x2/6 + x2*x2/120
  | otherwise  = sin x / x
  where
    ax    = abs x
    x2    = x*x
    -- For explanation of choice see `doc/sinc.hs'
    eps_0 = 1.8250120749944284e-8 -- sqrt (6ε/4)
    eps_2 = 1.4284346431400855e-4 --   (30ε)**(1/4) / 2
    eps_4 = 4.043633626430947e-3  -- (1206ε)**(1/6) / 2


----------------------------------------------------------------
-- Logarithm
----------------------------------------------------------------

-- | Compute log(1+x)-x:
log1pmx :: Double -> Double
log1pmx x
  | x <  -1        = error "Domain error"
  | x == -1        = m_neg_inf
  | ax > 0.95      = log(1 + x) - x
  | ax < m_epsilon = -(x * x) /2
  | otherwise      = - x * x * sumPowerSeries (-x) (recip <$> enumSequenceFrom 2)
  where
   ax = abs x

-- | /O(log n)/ Compute the logarithm in base 2 of the given value.
log2 :: Int -> Int
log2 v0
    | v0 <= 0   = modErr $ "log2: nonpositive input, got " ++ show v0
    | otherwise = go 5 0 v0
  where
    go !i !r !v | i == -1        = r
                | v .&. b i /= 0 = let si = U.unsafeIndex sv i
                                   in go (i-1) (r .|. si) (v `shiftR` si)
                | otherwise      = go (i-1) r v
    b = U.unsafeIndex bv
    !bv = U.fromList [ 0x02, 0x0c, 0xf0, 0xff00
                     , fromIntegral (0xffff0000 :: Word)
                     , fromIntegral (0xffffffff00000000 :: Word)]
    !sv = U.fromList [1,2,4,8,16,32]


----------------------------------------------------------------
-- Factorial
----------------------------------------------------------------

-- | Compute the factorial function /n/!.  Returns +∞ if the input is
--   above 170 (above which the result cannot be represented by a
--   64-bit 'Double').
factorial :: Int -> Double
factorial n
  | n < 0     = error "Numeric.SpecFunctions.factorial: negative input"
  | n > 170   = m_pos_inf
  | otherwise = U.unsafeIndex factorialTable n

-- | Compute the natural logarithm of the factorial function.  Gives
--   16 decimal digits of precision.
logFactorial :: Integral a => a -> Double
logFactorial n
  | n <  0    = error "Numeric.SpecFunctions.logFactorial: negative input"
  -- For smaller inputs we just look up table
  | n <= 170  = log $ U.unsafeIndex factorialTable (fromIntegral n)
  -- Otherwise we use asymptotic Stirling's series. Number of terms
  -- necessary depends on the argument.
  | n < 1500  = stirling + rx * ((1/12) - (1/360)*rx*rx)
  | otherwise = stirling + (1/12)*rx
  where
    stirling = (x - 0.5) * log x - x + m_ln_sqrt_2_pi
    x        = fromIntegral n + 1
    rx       = 1 / x
{-# SPECIALIZE logFactorial :: Int -> Double #-}


-- | Calculate the error term of the Stirling approximation.  This is
-- only defined for non-negative values.
--
-- \[
-- \operatorname{stirlingError}(n) = \log(n!) - \log(\sqrt{2\pi n}\frac{n}{e}^n)
-- \]
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

-- |
-- Quickly compute the natural logarithm of /n/ @`choose`@ /k/, with
-- no checking.
--
-- Less numerically stable:
--
-- > exp $ lg (n+1) - lg (k+1) - lg (n-k+1)
-- >   where lg = logGamma . fromIntegral
logChooseFast :: Double -> Double -> Double
logChooseFast n k = -log (n + 1) - logBeta (n - k + 1) (k + 1)

-- | Calculate binomial coefficient using exact formula
chooseExact :: Int -> Int -> Double
n `chooseExact` k
  = U.foldl' go 1 $ U.enumFromTo 1 k
  where
    go a i      = a * (nk + j) / j
        where j = fromIntegral i :: Double
    nk = fromIntegral (n - k)

-- | Compute logarithm of the binomial coefficient.
logChoose :: Int -> Int -> Double
n `logChoose` k
    | k  > n    = (-1) / 0
      -- For very large N exact algorithm overflows double so we
      -- switch to beta-function based one
    | k' < 50 && (n < 20000000) = log $ chooseExact n k'
    | otherwise                 = logChooseFast (fromIntegral n) (fromIntegral k)
  where
    k' = min k (n-k)

-- | Compute the binomial coefficient /n/ @\``choose`\`@ /k/. For
-- values of /k/ > 50, this uses an approximation for performance
-- reasons.  The approximation is accurate to 12 decimal places in the
-- worst case
--
-- Example:
--
-- > 7 `choose` 3 == 35
choose :: Int -> Int -> Double
n `choose` k
    | k  > n         = 0
    | k' < 50        = chooseExact n k'
    | approx < max64 = fromIntegral . round64 $ approx
    | otherwise      = approx
  where
    k'             = min k (n-k)
    approx         = exp $ logChooseFast (fromIntegral n) (fromIntegral k')
    max64          = fromIntegral (maxBound :: Int64)
    round64 x      = round x :: Int64

-- | Compute ψ(/x/), the first logarithmic derivative of the gamma
--   function.
--
-- \[
-- \psi(x) = \frac{d}{dx} \ln \left(\Gamma(x)\right) = \frac{\Gamma'(x)}{\Gamma(x)}
-- \]
--
-- Uses Algorithm AS 103 by Bernardo, based on Minka's C implementation.
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

factorialTable :: U.Vector Double
{-# NOINLINE factorialTable #-}
factorialTable = U.fromListN 171
  [ 1.0
  , 1.0
  , 2.0
  , 6.0
  , 24.0
  , 120.0
  , 720.0
  , 5040.0
  , 40320.0
  , 362880.0
  , 3628800.0
  , 3.99168e7
  , 4.790016e8
  , 6.2270208e9
  , 8.71782912e10
  , 1.307674368e12
  , 2.0922789888e13
  , 3.55687428096e14
  , 6.402373705728e15
  , 1.21645100408832e17
  , 2.43290200817664e18
  , 5.109094217170944e19
  , 1.1240007277776077e21
  , 2.5852016738884974e22
  , 6.204484017332394e23
  , 1.5511210043330984e25
  , 4.032914611266056e26
  , 1.0888869450418352e28
  , 3.0488834461171384e29
  , 8.841761993739702e30
  , 2.6525285981219103e32
  , 8.222838654177922e33
  , 2.631308369336935e35
  , 8.683317618811886e36
  , 2.9523279903960412e38
  , 1.0333147966386144e40
  , 3.719933267899012e41
  , 1.3763753091226343e43
  , 5.23022617466601e44
  , 2.0397882081197442e46
  , 8.159152832478977e47
  , 3.3452526613163803e49
  , 1.4050061177528798e51
  , 6.041526306337383e52
  , 2.6582715747884485e54
  , 1.1962222086548019e56
  , 5.5026221598120885e57
  , 2.5862324151116818e59
  , 1.2413915592536073e61
  , 6.082818640342675e62
  , 3.0414093201713376e64
  , 1.5511187532873822e66
  , 8.065817517094388e67
  , 4.2748832840600255e69
  , 2.308436973392414e71
  , 1.2696403353658275e73
  , 7.109985878048634e74
  , 4.0526919504877214e76
  , 2.3505613312828785e78
  , 1.386831185456898e80
  , 8.32098711274139e81
  , 5.075802138772247e83
  , 3.146997326038793e85
  , 1.9826083154044399e87
  , 1.2688693218588415e89
  , 8.24765059208247e90
  , 5.44344939077443e92
  , 3.647111091818868e94
  , 2.4800355424368305e96
  , 1.711224524281413e98
  , 1.197857166996989e100
  , 8.504785885678623e101
  , 6.1234458376886085e103
  , 4.470115461512684e105
  , 3.307885441519386e107
  , 2.4809140811395396e109
  , 1.88549470166605e111
  , 1.4518309202828586e113
  , 1.1324281178206297e115
  , 8.946182130782974e116
  , 7.15694570462638e118
  , 5.797126020747368e120
  , 4.753643337012841e122
  , 3.9455239697206583e124
  , 3.314240134565353e126
  , 2.81710411438055e128
  , 2.422709538367273e130
  , 2.1077572983795275e132
  , 1.8548264225739844e134
  , 1.650795516090846e136
  , 1.4857159644817613e138
  , 1.352001527678403e140
  , 1.2438414054641305e142
  , 1.1567725070816416e144
  , 1.087366156656743e146
  , 1.0329978488239058e148
  , 9.916779348709496e149
  , 9.619275968248211e151
  , 9.426890448883246e153
  , 9.332621544394413e155
  , 9.332621544394415e157
  , 9.425947759838358e159
  , 9.614466715035125e161
  , 9.902900716486179e163
  , 1.0299016745145626e166
  , 1.0813967582402908e168
  , 1.1462805637347082e170
  , 1.2265202031961378e172
  , 1.3246418194518288e174
  , 1.4438595832024934e176
  , 1.5882455415227428e178
  , 1.7629525510902446e180
  , 1.974506857221074e182
  , 2.2311927486598134e184
  , 2.543559733472187e186
  , 2.9250936934930154e188
  , 3.393108684451898e190
  , 3.9699371608087206e192
  , 4.68452584975429e194
  , 5.574585761207606e196
  , 6.689502913449126e198
  , 8.094298525273443e200
  , 9.875044200833601e202
  , 1.214630436702533e205
  , 1.5061417415111406e207
  , 1.8826771768889257e209
  , 2.372173242880047e211
  , 3.0126600184576594e213
  , 3.856204823625804e215
  , 4.974504222477286e217
  , 6.466855489220473e219
  , 8.471580690878819e221
  , 1.1182486511960041e224
  , 1.4872707060906857e226
  , 1.9929427461615188e228
  , 2.6904727073180504e230
  , 3.6590428819525483e232
  , 5.012888748274991e234
  , 6.917786472619488e236
  , 9.615723196941088e238
  , 1.3462012475717523e241
  , 1.898143759076171e243
  , 2.6953641378881624e245
  , 3.8543707171800725e247
  , 5.5502938327393044e249
  , 8.047926057471992e251
  , 1.1749972043909107e254
  , 1.7272458904546386e256
  , 2.5563239178728654e258
  , 3.808922637630569e260
  , 5.713383956445854e262
  , 8.62720977423324e264
  , 1.3113358856834524e267
  , 2.0063439050956823e269
  , 3.0897696138473508e271
  , 4.789142901463393e273
  , 7.471062926282894e275
  , 1.1729568794264143e278
  , 1.8532718694937346e280
  , 2.946702272495038e282
  , 4.714723635992061e284
  , 7.590705053947218e286
  , 1.2296942187394494e289
  , 2.0044015765453023e291
  , 3.287218585534296e293
  , 5.423910666131589e295
  , 9.003691705778436e297
  , 1.5036165148649988e300
  , 2.526075744973198e302
  , 4.269068009004705e304
  , 7.257415615307998e306
  ]
