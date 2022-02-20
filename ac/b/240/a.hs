solve :: Int -> Int -> String
solve 1 10 = "Yes"
solve a b 
  | b == a+1 = "Yes"
  | otherwise = "No"

main = do
  [a,b] <- map read.words <$> getLine :: IO [Int]
  putStrLn $solve a b
