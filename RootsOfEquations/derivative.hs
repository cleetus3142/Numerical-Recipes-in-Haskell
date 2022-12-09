-- This program is derived from  Scott Walck's book
-- "Learn Physics with Functional Programming"
-- See pages 37, 38 and following for computing derivatives.
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
