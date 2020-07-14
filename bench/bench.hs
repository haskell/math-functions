import Gauge.Main
import Data.Default.Class
import qualified Data.Vector.Unboxed as U
import Text.Printf

import Numeric.SpecFunctions
import Numeric.Polynomial
import Numeric.RootFinding



-- Uniformly sample logGamma performance between 10^-6 to 10^6
benchmarkLogGamma logG =
  [ bench (printf "%.3g" x) $ nf logG x
  | x <- [ m * 10**n | n <- [ -8 .. 8 ]
                     , m <- [ 10**(i / tics) | i <- [0 .. tics-1] ]
         ]
  ]
  where tics = 3
{-# INLINE benchmarkLogGamma #-}


-- Power of polynomial to be evaluated (In other words length of coefficients vector)
coef_size :: [Int]
coef_size = [ 1,2,3,4,5,6,7,8,9
            , 10,    30
            , 100,   300
            , 1000,  3000
            , 10000, 30000
            ]
{-# INLINE coef_size #-}

-- Precalculated coefficients
coef_list :: [U.Vector Double]
coef_list = [ U.replicate n 1.2 | n <- coef_size]
{-# NOINLINE coef_list #-}



main :: IO ()
main = defaultMain
  [ bgroup "logGamma" $
    benchmarkLogGamma logGamma
  , bgroup "incompleteGamma" $
      [ bench (show p) $ nf (incompleteGamma p) p
      | p <- [ 0.1
             , 1,   3
             , 10,  30
             , 100, 300
             , 999, 1000
             ]
      ]
  , bgroup "factorial"
    [ bench (show n) $ nf factorial n
    | n <- [ 0, 1, 3, 6, 9, 11, 15
           , 20, 30, 40, 50, 60, 70, 80, 90, 100
           ]
    ]
  , bgroup "incompleteBeta"
    [ bench (show (p,q,x)) $ nf (incompleteBeta p q) x
    | (p,q,x) <- [ (10,      10,      0.5)
                 , (101,     101,     0.5)
                 , (1010,    1010,    0.5)
                 , (10100,   10100,   0.5)
                 , (100100,  100100,  0.5)
                 , (1001000, 1001000, 0.5)
                 , (10010000,10010000,0.5)
                 ]
    ]
  , bgroup "log1p"
      [ bench (show x) $ nf log1p x
      | x <- [ -0.9
             , -0.5
             , -0.1
             ,  0.1
             ,  0.5
             ,  1
             ,  10
             ,  100
             ] :: [Double]
      ]
  , bgroup "sinc" $
        bench "sin" (nf sin (0.55 :: Double))
      : [ bench (show x) $ nf sinc x
        | x <- [0, 1e-6, 1e-3,  0.5]
        ]
  , bgroup "erf & Co"
    [ bgroup "erf"
      [ bench (show x) $ nf erf x
      | x <- [0, 1.1, 100, 1000]
      ]
    , bgroup "erfc"
      [ bench (show x) $ nf erfc x
      | x <- [0, 1.1, 100, 1000]
      ]
    , bgroup "invErfc"
      [ bench (show x) $ nf erfc x
      | x <- [1e-9, 1e-6, 1e-3, 0.1, 1]
      ]
    ]
  , bgroup "expm1"
    [ bench (show x) $ nf expm1 (x :: Double)
    | x <- [-0.1, 0, 1, 19]
    ]
  , bgroup "poly"
      $  [ bench ("vector_"++show (U.length coefs)) $ nf (\x -> evaluatePolynomial x coefs) (1 :: Double)
         | coefs <- coef_list ]
      ++ [ bench ("unpacked_"++show n) $ nf (\x -> evaluatePolynomialL x (map fromIntegral [1..n])) (1 :: Double)
         | n <- coef_size ]
  , bgroup "RootFinding"
    [ bench "ridders sin" $ nf (ridders       def (0,pi/2))     (\x -> sin x - 0.525)
    , bench "newton sin"  $ nf (newtonRaphson def (0,1.2,pi/2)) (\x -> (sin x - 0.525,cos x))
    ]
  ]
