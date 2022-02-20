insert x [] = [x]
insert x (y:ys)
  | x < y = x:y:ys
  | otherwise = y : insert x ys

insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

main = do 
  print $ insertionSort [1,5,3,8]
