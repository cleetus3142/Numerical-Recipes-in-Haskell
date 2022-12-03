fact :: Integer -> Integer
fact n = product [1..n]

-- This is the power series expansion of cos(x) = 1 - x^2/2! + x^4/4! ...
-- This is to 100 terms.
-- Use x^1 and not x**i. x^i is for integer exponents (i = 1,2,3  ... N)
-- x**i is for floating point exponents (i = 0.25, 0.333, 0.875 etc.)
-- See the section on Exponentiation and Type Classes in Scott Walck's
-- book "Functional Programming for Physics Geeks" (page 104)

cosine :: Double -> Double
cosine x  = sum [((-1)^i)*x^(2*i)/fromIntegral(fact(2*i)) | i <- [0..50]]
