-- Generate table for double factorials

factorials :: [Integer]
factorials = scanl (*) 1 [1..]

factorialsDouble :: [Double]
factorialsDouble
  = takeWhile (not . isInfinite)
  $ fmap fromInteger factorials

main :: IO ()
main = do
  putStrLn "  [ 1.0"
  mapM_ (\x -> putStrLn $ "  , " ++ show x) $ tail factorialsDouble
  putStrLn "  ]"
