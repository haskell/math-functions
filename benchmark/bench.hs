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
  ]
