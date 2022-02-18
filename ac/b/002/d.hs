
main :: IO ()
main = do 
  [n,r] <- map read.words <$> getLine
  ls <- map (map read.words) <$> replicateM r getLine
  print $ maximum.map length.filter (\x -> all (`elem` ls) [[i, j] | i <- x, j <- x, i < j]) $ subsequences [1..n]
