-- Computes the integral by Romberg Integration scheme
-- The integral is approximately I ~ I2n + 1/3*(I2n - In) = 4/3*I2n - 1/3*In
-- I is the desired integral, In is the integral for step size dt
-- I2n is the integral for dt/2.0 
-- BSD 2-Clause License Copyright -- (c) 2022, Jonathan Drews

type R = Double
type Integration = (R -> R) -> R -> R -> R

-- The trapezoid rule is the fundamental program for computing In
-- and I2n
integral :: R -> Integration
integral dt f a b = dt/2.0*(f(a)+f(b)) + sum [ f t * dt | t <- [a+dt, a+2*dt..b-dt]]

romb :: Int -> Integration
romb n f a b =
  let dt=(b-a)/fromIntegral(2*n)
  in 4.0/3.0*(integral (dt/2.0) f a b) - 1.0/3.0*(integral dt f a b)
  
