-- This program was copied from
-- https://rosettacode.org/wiki/Matrix_multiplication#Haskell
-- To use this program do:
{-
ghci> :l matrixMultiply.hs
[1 of 1] Compiling Main             ( matrixMultiply.hs, interpreted )
Ok, one module loaded.
ghci> a = [[1,2,3],[4,5,6],[7,8,9]]
ghci> b = [[1],[1],[1]]
ghci> mmult a b
[[6],[15],[24]]
ghci> 
-}

import Data.List

mmult :: Num a => [[a]] -> [[a]] -> [[a]] 
mmult a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]
