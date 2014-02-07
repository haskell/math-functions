import Criterion.Main
import Numeric.Sum as Sum
import System.Random.MWC
import qualified Data.Vector.Unboxed as U

main = do
  gen <- createSystemRandom
  v <- uniformVector gen 10000000 :: IO (U.Vector Double)
  defaultMain [
      bench "naive" $ whnf U.sum v
    , bench "pairwise" $ whnf pairwiseSum v
    , bench "kahan" $ whnf (sumVector kahan) v
    , bench "kbn" $ whnf (sumVector kbn) v
    , bench "kb2" $ whnf (sumVector kb2) v
    ]
