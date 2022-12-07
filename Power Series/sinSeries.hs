{-
BSD 2-Clause License

Copyright (c) 2022, Jonathan Drews

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

    Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE
, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

fact :: Integer -> Integer
fact n = product [1..n]

-- This is the power series expansion of sin(x) = x - x^3/3! + x^5/5! ...
-- This is to 100 terms.
-- Use x^1 and not x**i. x^i is for integer exponents (i = 1,2,3  ... N)
-- x**i is for floating point exponents (i = 0.25, 0.333, 0.875 etc.)
-- See the section on Exponentiation and Type Classes in Scott Walck's
-- book "Functional Programming for Physics Geeks" (page 104)

sine :: Double -> Double
sine x  = sum [((-1)^i)*x^(2*i+1)/fromIntegral(fact(2*i+1)) | i <- [0..50]]
