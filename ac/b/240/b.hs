remDup :: [Int] -> [Int]
remDup [] = []
remDup (x:xs) = x: remDup (remove x xs)

remove :: Int -> [Int] -> [Int]
remove x [] = []
remove x (y:ys)
  | x==y = remove x ys
  | otherwise = y:remove x ys
main=do
  _ <- getLine
  l <- Prelude.map read.words <$> getLine :: IO [Int]
  print.length$remDup l
