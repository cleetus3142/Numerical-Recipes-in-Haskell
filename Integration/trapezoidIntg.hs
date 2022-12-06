-- Computes the integral by the trapezoid rule
-- This is from Scott Walck's "Learn Physics with Functional Programming"
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
