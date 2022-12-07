-- Imported and modified from
-- https://rosettacode.org/wiki/Cramer%27s_rule#Haskell
-- See the ghciExamples section of this repository for an example of how to run this program

import Data.Matrix

solveCramer :: (Ord a, Fractional a) => Matrix a -> Matrix a -> Maybe [a]
solveCramer a y
  | da == 0 = Nothing
  | otherwise = Just $ map (\i -> d i / da) [1..n]
  where da = detLU a
        d i = detLU $ submatrix 1 n 1 n $ switchCols i (n+1) ay
        ay = a <|> y
        n = ncols a

task m b = solveCramer a y
  where a = fromLists m                      
        y = fromLists b
