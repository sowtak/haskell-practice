{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.Char

letters = map (\x -> chr (ord 'A' + (x-1) `mod` 26) . reverse . takeWhile (>0) . iterate (\x -> (x-1) div 26)
