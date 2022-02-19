solve :: Double -> Double
solve x = sqrt (x * (12800000 + x))

main = do
  r <- readLn
  print $ solve r
