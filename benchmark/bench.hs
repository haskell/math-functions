import Criterion.Main
import Numeric.SpecFunctions
import Text.Printf

main :: IO ()
main = defaultMain 
  [ bgroup "logGamma" 
    [ bench (printf "%.3f" x) $ nf logGamma x
    | x <- [ 0.1,0.4,0.8                  -- [0     .. 1.5  ]
           , 2, 3.5                       -- [1.5   .. 4    ]
           , 5, 7, 11                     -- [4     .. 12   ]
           , 100, 300, 1e3, 3e3, 1e4, 3e4 -- [12    .. 5.1e4]
           , 6e4                          -- [5.1e4 .. +∞   ]
           ]
    ]
  , bgroup "logGammaL"
    [ bench (printf "%.3f" x) $ nf logGammaL x
    | x <- [ 1e-1, 3e-1, 1e+0, 3e+0
           , 1e+1, 3e+1, 1e+2, 3e+2 
           , 1e+3, 3e+3, 1e+4, 3e+4 
           ]
    ]
  , bgroup "factorial"
    [ bench (show n) $ nf factorial n
    | n <- [ 0, 1, 3, 6, 9, 11, 15
           , 20, 30, 40, 50, 60, 70, 80, 90, 100
           ]
    ]
  ]
