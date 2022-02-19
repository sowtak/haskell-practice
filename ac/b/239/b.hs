{-# LANGUAGE TypeApplications #-}

main=do
  x <- readLn @Integer
  print $ x`div`10


