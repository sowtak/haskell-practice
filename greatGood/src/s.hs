removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

main :: IO ()
main = do
  print$removeNonUppercase "rEmOvE"
