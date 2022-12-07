-- Computes the integral by the trapezoid rule
-- This is from Scott Walck's "Learn Physics with Functional Programming"
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
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{- To run this program do:
$ ghci  
Loaded package environment from /home/cleetus/.ghc/x86_64-openbsd-9.2.4/environments/default
GHCi, version 9.2.4: https://www.haskell.org/ghc/  :? for help
ghci> :load trapezoidIntg.hs 
[1 of 1] Compiling Main             ( trapezoidIntg.hs, interpreted )
Ok, one module loaded.
ghci> integral 0.05 sqrd 0.0 3.0
9.001251
ghci> 
-}

sqrd :: Float -> Float
sqrd x = x^2

type R = Float
type Integration = (R -> R) -> R -> R -> R

integral :: R -> Integration
integral dt f a b = dt/2.0*(f(a)+f(b)) + sum [ f t * dt | t <- [a+dt, a+2*dt..b-dt]]
