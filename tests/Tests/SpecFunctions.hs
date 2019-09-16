{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}
-- | Tests for Statistics.Math
module Tests.SpecFunctions (
  tests
  ) where

import Control.Monad
import Data.List
import qualified Data.Vector as V
import           Data.Vector   ((!))

import Test.QuickCheck  hiding (choose,within)
import Test.Tasty
import Test.Tasty.QuickCheck   (testProperty)
import Test.Tasty.HUnit

import Tests.Helpers
import Tests.SpecFunctions.Tables
import Numeric.SpecFunctions
import Numeric.MathFunctions.Comparison (within,relativeError,ulpDistance)
import Numeric.MathFunctions.Constants  (m_epsilon,m_tiny)

erfTol,erfcTol,erfcLargeTol :: Int
#if USE_SYSTEM_ERF && !defined(__GHCJS__)
erfTol       = 1
erfcTol      = 2
erfcLargeTol = 2
#else
erfTol       = 4
erfcTol      = 4
erfcLargeTol = 64
#endif

tests :: TestTree
tests = testGroup "Special functions"
  [ testGroup "erf"
    [ -- implementation from numerical recipes loses presision for
      -- large arguments
      testCase "erfc table" $
        forTable "tests/tables/erfc.dat" $ \[x, exact] ->
          checkTabular erfcTol (show x) exact (erfc x)
    , testCase "erfc table [large]" $
        forTable "tests/tables/erfc-large.dat" $ \[x, exact] ->
          checkTabular erfcLargeTol (show x) exact (erfc x)
      --
    , testCase "erf table" $
        forTable "tests/tables/erf.dat" $ \[x, exact] -> do
          checkTabular erfTol (show x) exact (erf x)
    , testProperty "id = erfc . invErfc" invErfcIsInverse
    , testProperty "id = invErfc . erfc" invErfcIsInverse2
    , testProperty "invErf  = erf^-1"    invErfIsInverse
    ]
  --
  , testGroup "log1p & Co"
    [ testCase "expm1 table" $
        forTable "tests/tables/expm1.dat" $ \[x, exact] ->
          checkTabular 2 (show x) exact (expm1 x)
    , testCase "log1p table" $
        forTable "tests/tables/log1p.dat" $ \[x, exact] ->
          checkTabular 1 (show x) exact (log1p x)
    ]
  ----------------
  , testGroup "gamma function"
    [ testCase "logGamma table [fractional points" $
        forTable "tests/tables/loggamma.dat" $ \[x, exact] -> do
          checkTabular 80 (show x) exact (logGamma x)
    , testProperty "Gamma(x+1) = x*Gamma(x)" $ gammaReccurence
    , testCase     "logGamma is expected to be precise at 1e-15 level" $
        forM_ [3..10000::Int] $ \n -> do
          let exact = logFactorial (n-1)
              val   = logGamma (fromIntegral n)
          checkTabular 8 (show n) exact val
    ]
  ----------------
  , testGroup "incomplete gamma"
    [ testCase "incompleteGamma table" $
        forTable "tests/tables/igamma.dat" $ \[a,x,exact] -> do
          checkTabular 16 (show (a,x)) exact (incompleteGamma a x)
    , testProperty "incomplete gamma - increases" $
        \(abs -> s) (abs -> x) (abs -> y) -> s > 0 ==> monotonicallyIncreases (incompleteGamma s) x y
    , testProperty "0 <= gamma <= 1"               incompleteGammaInRange
    , testProperty "gamma(1,x) = 1 - exp(-x)"      incompleteGammaAt1Check
    , testProperty "invIncompleteGamma = gamma^-1" invIGammaIsInverse
    ]
  ----------------
  , testGroup "beta function"
    [ testCase "logBeta table" $
        forTable "tests/tables/logbeta.dat" $ \[p,q,exact] ->
          checkTabular 256 (show (p,q)) (logBeta p q) exact
    ]
  ----------------
  , testGroup "incomplete beta"
    [ testCase "incompleteBeta table" $
        forM_ tableIncompleteBeta $ \(p,q,x,exact) ->
          checkTabular 64 (show (x,p,q)) (incompleteBeta p q x) exact
    , testCase "incompleteBeta table with p > 3000 and q > 3000" $
        forM_ tableIncompleteBetaP3000 $ \(x,p,q,exact) ->
          checkTabular 7000 (show (x,p,q)) (incompleteBeta p q x) exact
    --
    , testProperty "0 <= I[B] <= 1" incompleteBetaInRange
    -- XXX FIXME DISABLED due to failures
    -- , testProperty "invIncompleteBeta  = B^-1" $ invIBetaIsInverse
    ]
  ----------------
  , testGroup "digamma"
    [ testAssertion "digamma is expected to be precise at 1e-14 [integers]"
        $ digammaTestIntegers 1e-14
      -- Relative precision is lost when digamma(x) ≈ 0
    , testCase "digamma is expected to be precise at 1e-12" $
      forTable "tests/tables/digamma.dat" $ \[x, exact] ->
        checkTabular 2048
          (show x) (digamma x) exact
    ]
  ----------------
  , testGroup "factorial"
    [ testCase "Factorial table" $
      forM_ [0 .. 170] $ \n -> do
        checkTabular 6
          (show n)
          (fromIntegral (factorial' n))
          (factorial (fromIntegral n :: Int))
      --
    , testCase "Log factorial table" $
      forM_ [2 .. 170] $ \n -> do
        checkTabular 3
          (show n)
          (log $ fromIntegral $ factorial' n)
          (logFactorial (fromIntegral n :: Int))
    ]
  ----------------
  , testGroup "combinatorics"
    [ testCase "choose table" $
      forM_ [0 .. 1000] $ \n ->
        forM_ [0 .. n]  $ \k -> do
          checkTabular 2048
            (show (n,k))
            (fromIntegral $ choose' n k)
            (choose (fromInteger n) (fromInteger k))
    --
    , testCase "logChoose == log . choose" $
      forM_ [0 .. 1000] $ \n ->
        forM_ [0 .. n]  $ \k -> do
          checkTabular 2
            (show (n,k))
            (log $ choose n k)
            (logChoose n k)
    ]
    ----------------------------------------------------------------
    -- Self tests
  , testGroup "self-test"
    [ testProperty "Self-test: 0 <= range01 <= 1" $ \x -> let f = range01 x in f <= 1 && f >= 0
    ]
  ]

----------------------------------------------------------------
-- efr tests
----------------------------------------------------------------

roundtrip_erfc_invErfc,
  roundtrip_invErfc_erfc,
  roundtrip_erf_invErf
  :: (Double,Double)
#if USE_SYSTEM_ERF && !defined(__GHCJS__)
roundtrip_erfc_invErfc = (2,2)
roundtrip_invErfc_erfc = (2,2)
roundtrip_erf_invErf   = (1,1)
#else
roundtrip_erfc_invErfc = (2,8)
roundtrip_invErfc_erfc = (8,4)
roundtrip_erf_invErf   = (128,128)
#endif

-- id ≈ erfc . invErfc
invErfcIsInverse :: Double -> Property
invErfcIsInverse ((*2) . range01 -> x)
  = (not $ isInfinite x) ==>
  ( counterexample ("x        = " ++ show x )
  $ counterexample ("y        = " ++ show y )
  $ counterexample ("x'       = " ++ show x')
  $ counterexample ("calc.err = " ++ show (delta, delta-e'))
  $ counterexample ("ulps     = " ++ show (ulpDistance x x'))
  $ ulpDistance x x' <= round delta
  )
  where
    (e,e') = roundtrip_erfc_invErfc
    delta  = e' + e * abs ( y / x  *  2 / sqrt pi * exp( -y*y ))
    y  = invErfc x
    x' = erfc y

-- id ≈ invErfc . erfc
invErfcIsInverse2 :: Double -> Property
invErfcIsInverse2 x
  = (not $ isInfinite x') ==>
    (y > m_tiny)          ==>
    (x /= 0) ==>
    counterexample ("x        = " ++ show x )
  $ counterexample ("y        = " ++ show y )
  $ counterexample ("x'       = " ++ show x')
  $ counterexample ("calc.err = " ++ show delta)
  $ counterexample ("ulps     = " ++ show (ulpDistance x x'))
  $ ulpDistance x x' <= delta
  where
    (e,e') = roundtrip_invErfc_erfc
    delta  = round
           $ e' + e * abs (y / x  /  (2 / sqrt pi * exp( -x*x )))
    y  = erfc x
    x' = invErfc y

-- id ≈ erf . invErf
invErfIsInverse :: Double -> Property
invErfIsInverse a
  = (x /= 0) ==>
    counterexample ("x        = " ++ show x )
  $ counterexample ("y        = " ++ show y )
  $ counterexample ("x'       = " ++ show x')
  $ counterexample ("calc.err = " ++ show delta)
  $ counterexample ("ulps     = " ++ show (ulpDistance x x'))
  $ ulpDistance x x' <= delta
  where
    (e,e') = roundtrip_erf_invErf
    delta  = round
           $ e + e' * abs (y / x  *  2 / sqrt pi * exp ( -y * y ))
    x  | a < 0     = - range01 a
       | otherwise =   range01 a
    y  = invErf x
    x' = erf y

----------------------------------------------------------------
-- QC tests
----------------------------------------------------------------

-- Γ(x+1) = x·Γ(x)
gammaReccurence :: Double -> Property
gammaReccurence x
  = x > 0  ==>  err < errEst
    where
      g1     = logGamma x
      g2     = logGamma (x+1)
      err    = abs (g2 - g1 - log x)
      -- logGamma apparently is not as precise for small x. See #59 for details 
      errEst = max 1e-14
             $ 2 * m_epsilon * sum (map abs [ g1 , g2 , log x ])

-- γ(s,x) is in [0,1] range
incompleteGammaInRange :: Double -> Double -> Property
incompleteGammaInRange (abs -> s) (abs -> x) =
  x >= 0 && s > 0  ==> let i = incompleteGamma s x in i >= 0 && i <= 1

-- γ(1,x) = 1 - exp(-x)
-- Since Γ(1) = 1 normalization doesn't make any difference
incompleteGammaAt1Check :: Double -> Bool
incompleteGammaAt1Check (abs -> x) =
  ulpDistance (incompleteGamma 1 x) (-expm1(-x)) < 16

-- invIncompleteGamma is inverse of incompleteGamma
invIGammaIsInverse :: Double -> Double -> Property
invIGammaIsInverse (abs -> a) (range01 -> p) =
  a > m_tiny && p > m_tiny && p < 1 && x > m_tiny  ==>
    ( counterexample ("a    = " ++ show a )
    $ counterexample ("p    = " ++ show p )
    $ counterexample ("x    = " ++ show x )
    $ counterexample ("p'   = " ++ show p')
    $ counterexample ("err  = " ++ show (ulpDistance p p'))
    $ counterexample ("est  = " ++ show est)
    $ ulpDistance p p' <= est
    )
  where
    x  = invIncompleteGamma a p
    f' = exp ( log x * (a-1) - x - logGamma a)
    p' = incompleteGamma    a x
    -- FIXME: Test should be rechecked when #42 is fixed
    (e,e') = (32,32)
    est    = round
           $ e' + e * abs (x / p * f')

-- B(s,x) is in [0,1] range
incompleteBetaInRange :: Double -> Double -> Double -> Property
incompleteBetaInRange (abs -> p) (abs -> q) (range01 -> x) =
  p > 0 && q > 0  ==> let i = incompleteBeta p q x in i >= 0 && i <= 1

-- invIncompleteBeta is inverse of incompleteBeta
invIBetaIsInverse :: Double -> Double -> Double -> Property
invIBetaIsInverse (abs -> p) (abs -> q) (range01 -> x) =
  p > 0 && q > 0  ==> ( counterexample ("p   = " ++ show p )
                      $ counterexample ("q   = " ++ show q )
                      $ counterexample ("x   = " ++ show x )
                      $ counterexample ("x'  = " ++ show x')
                      $ counterexample ("a   = " ++ show a)
                      $ counterexample ("err = " ++ (show $ abs $ (x - x') / x))
                      $ abs (x - x') <= 1e-12
                      )
  where
    x' = incompleteBeta    p q a
    a  = invIncompleteBeta p q x

-- Table for digamma function:
--
-- Uses equality ψ(n) = H_{n-1} - γ where
--   H_{n} = Σ 1/k, k = [1 .. n]     - harmonic number
--   γ     = 0.57721566490153286060  - Euler-Mascheroni number
digammaTestIntegers :: Double -> Bool
digammaTestIntegers eps
  = all (uncurry $ eq eps) $ take 3000 digammaInt
  where
    ok approx exact = approx
    -- Harmonic numbers starting from 0
    harmN = scanl (\a n -> a + 1/n) 0 [1::Rational .. ]
    gam   = 0.57721566490153286060
    -- Digamma values
    digammaInt = zipWith (\i h -> (digamma i, realToFrac h - gam)) [1..] harmN


----------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------

-- Lookup table for fact factorial calculation. It has fixed size
-- which is bad but it's OK for this particular case
factorial_table :: V.Vector Integer
factorial_table = V.generate 2000 (\n -> product [1..fromIntegral n])

-- Exact implementation of factorial
factorial' :: Integer -> Integer
factorial' n = factorial_table ! fromIntegral n

-- Exact albeit slow implementation of choose
choose' :: Integer -> Integer -> Integer
choose' n k = factorial' n `div` (factorial' k * factorial' (n-k))

-- Truncate double to [0,1]
range01 :: Double -> Double
range01 = abs . (snd :: (Integer, Double) -> Double) . properFraction


readTable :: FilePath -> IO [[Double]]
readTable
  = fmap (fmap (fmap read . words) . lines)
  . readFile

forTable :: FilePath -> ([Double] -> IO ()) -> IO ()
forTable path fun = do
  mapM_ fun =<< readTable path

checkTabular :: Int -> String -> Double -> Double -> IO ()
checkTabular prec x exact val =
  assertBool (unlines [ " x         = " ++ x
                      , " expected  = " ++ show exact
                      , " got       = " ++ show val
                      , " ulps diff = " ++ show (ulpDistance exact val)
                      ])
    (within prec exact val)
