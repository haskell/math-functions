import Criterion.Main
import qualified Data.Vector.Unboxed as U
import Numeric.SpecFunctions
import Numeric.Polynomial
import Text.Printf

-- Uniformly sample logGamma performance between 10^-6 to 10^6
benchmarkLogGamma logG =
  [ bench (printf "%.3g" x) $ nf logG x
  | x <- [ m * 10**n | n <- [ -8 .. 8 ]
                     , m <- [ 10**(i / tics) | i <- [0 .. tics-1] ]
         ]
  ]
  where tics = 3
{-# INLINE benchmarkLogGamma #-}

coefs_1,coefs_10,coefs_100,coefs_1000 :: U.Vector Double
coefs_1    = U.replicate 1    1.4
coefs_10   = U.replicate 10   1.2
coefs_100  = U.replicate 100  1.2
coefs_1000 = U.replicate 1000 1.2
{-# NOINLINE coefs_1    #-}
{-# NOINLINE coefs_10   #-}
{-# NOINLINE coefs_100  #-}
{-# NOINLINE coefs_1000 #-}

main :: IO ()
main = defaultMain 
  [ bgroup "logGamma" $
    benchmarkLogGamma logGamma
  , bgroup "logGammaL" $
    benchmarkLogGamma logGammaL
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
             ]
      ]
  , bgroup "poly"
      [ bench "vector_1"      $ nf (\x -> evaluatePolynomial x coefs_1  ) 1
      , bench "vector_10"     $ nf (\x -> evaluatePolynomial x coefs_10 ) 1
      , bench "vector_100"    $ nf (\x -> evaluatePolynomial x coefs_100) 1
      , bench "vector_1000"   $ nf (\x -> evaluatePolynomial x coefs_1000) 1
      , bench "unpacked_1"    $ nf (\x -> evaluatePolynomialL x [1..1]   ) (1::Double)
      , bench "unpacked_10"   $ nf (\x -> evaluatePolynomialL x [1..10]  ) (1::Double)
      , bench "unpacked_100"  $ nf (\x -> evaluatePolynomialL x [1..100] ) (1::Double)
      , bench "unpacked_1000" $ nf (\x -> evaluatePolynomialL x [1..1000]) (1::Double)
      ]
  ]
