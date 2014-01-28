import Test.Framework       (defaultMain)
import qualified Tests.SpecFunctions
import qualified Tests.Chebyshev
import qualified Tests.Sum

main :: IO ()
main = defaultMain [ Tests.SpecFunctions.tests
                   , Tests.Chebyshev.tests
                   , Tests.Sum.tests
                   ]
