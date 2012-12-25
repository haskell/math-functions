import Criterion.Main
import Numeric.SpecFunctions
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

main :: IO ()
main = defaultMain 
  [ bgroup "logGamma" $
    benchmarkLogGamma logGamma
  , bgroup "logGammaL" $
    benchmarkLogGamma logGammaL
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
  ]
