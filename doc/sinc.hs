-- Description of choice of approximation boundaries in sinc function
module Sinc where

import Numeric.MathFunctions.Constants (m_epsilon)


-- Approximations for sinc up to 6th order and "exact" implementation
f2,f4,f6,f :: Double -> Double
f2 x = 1 - x*x/6
f4 x = 1 - x*x/6 + x*x*x*x/120
f6 x = 1 - x*x/6 + x*x*x*x/120 - x*x*x*x*x*x/5040
f  x = sin x / x

-- When next term becomes so small that (1-e)==1 we can neglect it:
e0,e2,e4 :: Double
e0 = sqrt (6 * m_epsilon / 4)
e2 = (30   * m_epsilon) ** (1/4) / 2
e4 = (1260 * m_epsilon) ** (1/6) / 2

test :: IO ()
test = do
  print ("e0",e0)
  print $ f  e0 == 1
  print $ f2 e0 == 1
  --
  print ("e2",e2)
  print $ f  e2 == f2 e2
  print $ f2 e2 == f4 e2
  --
  print ("e4",e4)
  print $ f  e4 == f4 e4
  print $ f4 e4 == f6 e4
  
