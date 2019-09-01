{-# LANGUAGE ViewPatterns #-}
-- | Tests for Statistics.Math
module Tests.SpecFunctions (
  tests
  ) where

import Control.Monad
import qualified Data.Vector as V
import           Data.Vector   ((!))

import Test.QuickCheck  hiding (choose,within)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool)

import Tests.Helpers
import Tests.SpecFunctions.Tables
import Numeric.SpecFunctions
import Numeric.MathFunctions.Comparison (within,relativeError,ulpDistance)
import Numeric.MathFunctions.Constants  (m_epsilon,m_tiny)

tests :: Test
tests = testGroup "Special functions"
  [ testProperty "Gamma(x+1) = x*Gamma(x) [logGamma]"  $ gammaReccurence logGamma  3e-8
  , testProperty "Gamma(x+1) = x*Gamma(x) [logGammaL]" $ gammaReccurence logGammaL 2e-13
  , testProperty "gamma(1,x) = 1 - exp(-x)"      $ incompleteGammaAt1Check
  , testProperty "0 <= gamma <= 1"               $ incompleteGammaInRange
  , testProperty "0 <= I[B] <= 1"            $ incompleteBetaInRange
  , testProperty "invIncompleteGamma = gamma^-1" $ invIGammaIsInverse
  -- XXX FIXME DISABLED due to failures
  -- , testProperty "invIncompleteBeta  = B^-1" $ invIBetaIsInverse
  , testProperty "gamma - increases" $
      \(abs -> s) (abs -> x) (abs -> y) -> s > 0 ==> monotonicallyIncreases (incompleteGamma s) x y
  , testProperty "invErfc = erfc^-1"         $ invErfcIsInverse
  , testProperty "invErf  = erf^-1"          $ invErfIsInverse
  -- Tests for erfc mostly are to test implementation bundled with
  -- library. libc's one is accurate within 1 ulp
  , testCase "erfc table" $ forM_ tableErfc $ \(x,exact) -> do
      let val = erfc x
      assertBool (unlines [ " x         = " ++ show x
                          , " expected  = " ++ show exact
                          , " got       = " ++ show val
                          , " ulps diff = " ++ show (ulpDistance exact val)
                          ])
        (within 64 exact val)
  , testCase "erf table" $ forM_ tableErf $ \(x,exact) -> do
      let val = erf x
      assertBool (unlines [ " x         = " ++ show x
                          , " expected  = " ++ show exact
                          , " got       = " ++ show val
                          , " ulps diff = " ++ show (ulpDistance exact val)
                          ])
        (within 24 exact val)
    -- Unit tests
  , testAssertion "Factorial is expected to be precise at 1e-15 level"
      $ and [ eq 1e-15 (factorial (fromIntegral n :: Int))
                       (fromIntegral (factorial' n))
            |n <- [0..170]]
  , testAssertion "Log factorial is expected to be precise at 1e-15 level"
      $ and [ eq 1e-15 (logFactorial (fromIntegral n :: Int))
                       (log $ fromIntegral $ factorial' n)
            | n <- [2..170]]
  , testAssertion "logGamma is expected to be precise at 1e-9 level [integer points]"
      $ and [ eq 1e-9 (logGamma (fromIntegral n))
                      (logFactorial (n-1))
            | n <- [3..10000::Int]]
  , testAssertion "logGamma is expected to be precise at 1e-9 level [fractional points]"
      $ and [ eq 1e-9 (logGamma x) lg | (x,lg) <- tableLogGamma ]
  , testAssertion "logGammaL is expected to be precise at 1e-15 level"
      $ and [ eq 1e-15 (logGammaL (fromIntegral n))
                       (logFactorial (n-1))
            | n <- [3..10000::Int]]
  , testAssertion "logGammaL is expected to be precise at 1e-10 level [fractional points]"
      $ and [ eq (64*m_epsilon) (logGammaL x) lg | (x,lg) <- tableLogGamma ]
    -- FIXME: loss of precision when logBeta p q ≈ 0.
    --        Relative error doesn't work properly in this case.
  , testAssertion "logBeta is expected to be precise at 1e-6 level"
      $ and [ eq 1e-6 (logBeta p q)
                      (logGammaL p + logGammaL q - logGammaL (p+q))
            | p <- [0.1,0.2 .. 0.9] ++ [2 .. 20]
            , q <- [0.1,0.2 .. 0.9] ++ [2 .. 20]
            ]
  , testAssertion "digamma is expected to be precise at 1e-14 [integers]"
      $ digammaTestIntegers 1e-14
    -- Relative precision is lost when digamma(x) ≈ 0
  , testAssertion "digamma is expected to be precise at 1e-12"
      $ and [ eq 1e-12 r (digamma x) | (x,r) <- tableDigamma ]
    --
  , let deviations = [ ( "p=",p, "q=",q, "x=",x
                       , "ib=",ib, "ib'=",ib'
                       , "err=",relativeError ib ib' / m_epsilon)
                     | (p,q,x,ib) <- tableIncompleteBeta
                     , let ib' = incompleteBeta p q x
                     , not $ eq (64 * m_epsilon) ib' ib
                     ]
    in testCase "incompleteBeta is expected to be precise at 32*m_epsilon level"
     $ assertBool (unlines (map show deviations)) (null deviations)
  , testAssertion "incompleteBeta with p > 3000 and q > 3000"
      $ and [ eq 1e-11 (incompleteBeta p q x) ib | (x,p,q,ib) <-
                 [ (0.495,  3001,  3001, 0.2192546757957825068677527085659175689142653854877723)
                 , (0.501,  3001,  3001, 0.5615652382981522803424365187631195161665429270531389)
                 , (0.531,  3500,  3200, 0.9209758089734407825580172472327758548870610822321278)
                 , (0.501, 13500, 13200, 0.0656209987264794057358373443387716674955276089622780)
                 ]
            ]
  , testAssertion "choose is expected to precise at 1e-12 level"
      $ and [ eq 1e-12 (choose (fromIntegral n) (fromIntegral k)) (fromIntegral $ choose' n k)
            | n <- [0..1000], k <- [0..n]]
  , testAssertion "logChoose == log . choose"
      $ and [ let n' = fromIntegral n
                  k' = fromIntegral k
              in within 2 (logChoose n' k') (log $ choose n' k')
            | n <- [0::Int .. 1000], k <- [0 .. n]]
    ----------------------------------------------------------------
    -- Self tests
  , testProperty "Self-test: 0 <= range01 <= 1" $ \x -> let f = range01 x in f <= 1 && f >= 0
  ]

----------------------------------------------------------------
-- QC tests
----------------------------------------------------------------

-- Γ(x+1) = x·Γ(x)
gammaReccurence :: (Double -> Double) -> Double -> Double -> Property
gammaReccurence logG ε x =
  (x > 0 && x < 100)  ==>  (abs (g2 - g1 - log x) < ε)
    where
      g1 = logG x
      g2 = logG (x+1)

-- γ(s,x) is in [0,1] range
incompleteGammaInRange :: Double -> Double -> Property
incompleteGammaInRange (abs -> s) (abs -> x) =
  x >= 0 && s > 0  ==> let i = incompleteGamma s x in i >= 0 && i <= 1

-- γ(1,x) = 1 - exp(-x)
-- Since Γ(1) = 1 normalization doesn't make any difference
incompleteGammaAt1Check :: Double -> Property
incompleteGammaAt1Check (abs -> x) =
  x > 0 ==> (incompleteGamma 1 x + exp(-x)) ≈ 1
  where
    (≈) = eq 1e-13

-- invIncompleteGamma is inverse of incompleteGamma
invIGammaIsInverse :: Double -> Double -> Property
invIGammaIsInverse (abs -> a) (range01 -> p) =
  a > m_tiny && p > m_tiny && p < 1  ==>
    ( counterexample ("a    = " ++ show a )
    $ counterexample ("p    = " ++ show p )
    $ counterexample ("x    = " ++ show x )
    $ counterexample ("p'   = " ++ show p')
    $ counterexample ("err  = " ++ show (relativeError p p'))
    $ counterexample ("pred = " ++ show δ)
    $ relativeError p p' < δ
    )
  where
    x  = invIncompleteGamma a p
    f' = exp ( log x * (a-1) - x - logGamma a)
    p' = incompleteGamma    a x
    -- FIXME: 128 is big constant. It should be replaced by something
    --        smaller when #42 is fixed
    δ  = (m_epsilon/2) * (256 + 1 * (1 + abs (x * f' / p)))

-- invErfc is inverse of erfc
invErfcIsInverse :: Double -> Property
invErfcIsInverse ((*2) . range01 -> p)
  = counterexample ("p  = " ++ show p )
  $ counterexample ("x  = " ++ show x )
  $ counterexample ("p' = " ++ show p')
  $ abs (p - p') <= 1e-14
  where
    x  = invErfc p
    p' = erfc x

-- invErf is inverse of erf
invErfIsInverse :: Double -> Property
invErfIsInverse a
  = counterexample ("p  = " ++ show p )
  $ counterexample ("x  = " ++ show x )
  $ counterexample ("p' = " ++ show p')
  $ abs (p - p') <= 1e-14
  where
    x  = invErf p
    p' = erf x
    p  | a < 0     = - range01 a
       | otherwise =   range01 a

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


-- Table of values for erfc.
--
-- Values are computed using python's mpmath up to 30 significant
-- digits
tableErfc :: [(Double,Double)]
tableErfc =
  [ (0.000000, 1.0)
  , (0.020000, 0.977435425308155055306039814223)
  , (0.040000, 0.954888893854875246972188637445)
  , (0.060000, 0.932378405606691560417070009221)
  , (0.080000, 0.909921874158981837409467036376)
  , (0.100000, 0.887537083981715101595287748986)
  , (0.200000, 0.777297410789521533823546968791)
  , (0.300000, 0.671373240540872583810382014682)
  , (0.400000, 0.571607644953331523545890372692)
  , (0.500000, 0.479500122186953462317253346108)
  , (0.600000, 0.396143909152074094917693241426)
  , (0.700000, 0.322198806162581557723141845649)
  , (0.800000, 0.257899035292339487410212644387)
  , (0.900000, 0.20309178757716786033533383966)
  , (1.000000, 0.157299207050285130658779364917)
  , (1.100000, 0.119794930425918270342740490744)
  , (1.200000, 0.0896860217703646316340682061529)
  , (1.300000, 0.0659920550593475541498146384224)
  , (1.400000, 0.047714880237351203600376783391)
  , (1.500000, 0.0338948535246892729330237383541)
  , (1.600000, 0.0236516166553559844782198079153)
  , (1.700000, 0.0162095414092254391586870541911)
  , (1.800000, 0.0109094983642692838537604396016)
  , (1.900000, 0.0072095707647425327627840328679)
  , (2.000000, 0.00467773498104726583793074363275)
  , (2.0009765625, 0.00465759175242884900812001805563)
  , (2.100000, 0.00297946665633298428569058244218)
  , (2.200000, 0.00186284629798188985855863885328)
  , (2.300000, 0.00114317659735665247591992820336)
  , (2.400000, 0.000688513896645078885549974809715)
  , (2.500000, 0.000406952017444958939564215739975)
  , (3.000000, 0.0000220904969985854413727761295823)
  , (3.500000, 0.000000743098372341412745523683756096)
  , (11.000000, 1.44086613794369468033980970286e-54)
  , (23.000000, 4.44126594808805724407488442895e-232)
  ]
tableErf :: [(Double,Double)]
tableErf =
  [ (0.000000, 0.0)
  , (0.020000, 0.0225645746918449446939601857765)
  , (0.040000, 0.0451111061451247530278113625549)
  , (0.060000, 0.0676215943933084395829299907792)
  , (0.080000, 0.0900781258410181625905329636245)
  , (0.100000, 0.112462916018284898404712251014)
  , (0.200000, 0.222702589210478466176453031209)
  , (0.300000, 0.328626759459127416189617985318)
  , (0.400000, 0.428392355046668476454109627308)
  , (0.500000, 0.520499877813046537682746653892)
  , (0.600000, 0.603856090847925905082306758574)
  , (0.700000, 0.677801193837418442276858154351)
  , (0.800000, 0.742100964707660512589787355613)
  , (0.900000, 0.79690821242283213966466616034)
  , (1.000000, 0.842700792949714869341220635083)
  , (1.100000, 0.880205069574081729657259509256)
  , (1.200000, 0.910313978229635368365931793847)
  , (1.300000, 0.934007944940652445850185361578)
  , (1.400000, 0.952285119762648796399623216609)
  , (1.500000, 0.966105146475310727066976261646)
  , (1.600000, 0.976348383344644015521780192085)
  , (1.700000, 0.983790458590774560841312945809)
  , (1.800000, 0.989090501635730716146239560398)
  , (1.900000, 0.992790429235257467237215967132)
  , (2.000000, 0.995322265018952734162069256367)
  , (2.100000, 0.997020533343667015714309417558)
  , (2.200000, 0.998137153702018110141441361147)
  , (2.300000, 0.998856823402643347524080071797)
  , (2.400000, 0.99931148610335492111445002519)
  , (2.500000, 0.99959304798255504106043578426)
  , (3.000000, 0.99997790950300141455862722387)
  ]
