-- Newton's method for finding roots. Give a reasonable guess
-- for t. Below is an example. This program is derived from
-- Scott Walck's book "Learn Physics with Functional Programming"
-- See pages 37, 38 and following. Newtons method usually converges
-- to a solution quickly. Usually within 2 to 4 tries. 

{-
GHCi, version 9.2.4: https://www.haskell.org/ghc/  :? for help
ghci> :l newtonsMethod.hs
[1 of 1] Compiling Main             ( newtonsMethod.hs, interpreted )
ghci> -- First  guess of 1.5
ghci> newton 0.001 cos 1.5
1.5709148472574452
ghci> -- Second guess of 1,570915
ghci> newton 0.001 cos 1.570915
1.5707963267893947
ghci> -- Third guess of 1.5708
ghci> newton 0.001 cos 1.5708
1.5707963267947436
ghci> -- It converged. Below is the exact answer
ghci> pi/2.0
1.5707963267948966
ghci>
-}
-- BSD 2-Clause License Copyright -- (c) 2022, Jonathan Drews

type R = Double

-- dt is the step size for the derivative of f. f is the
-- function. t is where the derivative is evaluated at.
-- Don't make dt too small (i.e. 0.0000001). Usually dt=0.001
-- is fine. See the section  beginning on page 45 of Scott
-- Walck's book, Approximate Algorithms and Finite Precision

type Derivative = R -> (R -> R) -> (R -> R)

deriv :: Derivative
deriv dt f t = (f(t + dt/2) - f(t - dt/2))/dt

newton ::  Derivative
newton dt f t = t - (f t)/(deriv dt f t)
