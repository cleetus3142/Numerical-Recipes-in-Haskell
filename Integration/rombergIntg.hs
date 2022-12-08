-- Computes the integral by Romberg Integration scheme
-- The integral is approximately I ~ I2n + 1/3*(I2n - In) = 4/3*I2n - 1/3*IN
-- I is the desired integral, In is the integral for step size dt
-- I2n is the integral for dt/2.0
-- romb uses the Trapezoid rule program trapezoidIntg.hs 
-- BSD 2-Clause License Copyright -- (c) 2022, Jonathan Drews

type R = Double
type Integration = (R -> R) -> R -> R -> R

integral :: R -> Integration
integral dt f a b = dt/2.0*(f(a)+f(b)) + sum [ f t * dt | t <- [a+dt, a+2*dt..b-dt]]

romb :: R -> Integration
romb dt f a b =  4.0/3.0*(integral (dt/2.0) f a b) - 1.0/3.0*(integral dt f a b)
