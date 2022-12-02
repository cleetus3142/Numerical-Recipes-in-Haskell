-- Computes the integral by simpsons rule: I ~ 4/3*dt*(f(a+dt) + f(a+3*dt) + ...
-- f(b-dt)) + 2/3*dt*(f(a+2*dt) + f(a+4*dt) + ... + f(b-2*dt)) +
-- dt/3*(f(a) + f(b))

sqrd :: Double -> Double
sqrd x = x^2

type R = Double
type Integration = (R -> R) -> R -> R -> R

integral :: R -> Integration
integral dt f a b = dt/3.0*(f(a)+f(b)) +
 4.0/3.0*(sum [ f t * dt | t <- [a+dt, a+3*dt..b-dt]]) +
 2.0/3.0*(sum [ f t * dt | t <- [a+2*dt, a+4*dt..b-2*dt]])
 
