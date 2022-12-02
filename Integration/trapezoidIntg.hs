-- Computes the integral by the trapezoid rule

sqrd :: Float -> Float
sqrd x = x^2

type R = Float
type Integration = (R -> R) -> R -> R -> R

integral :: R -> Integration
integral dt f a b = dt/2.0*(f(a)+f(b)) + sum [ f t * dt | t <- [a+dt, a+2*dt..b-dt]]
