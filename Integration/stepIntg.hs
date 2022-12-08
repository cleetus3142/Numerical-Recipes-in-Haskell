-- Computes the integrl by step functions. From	Scott Walck's book
-- "Learn Physics with Functional Programming" page 84
-- BSD 2-Clause License Copyright -- (c) 2022, Jonathan Drews

sqrd :: Float -> Float
sqrd x = x^2

type R = Float
type Integration = (R -> R) -> R -> R -> R

integral :: R -> Integration
integral dt f a b = sum [ f t * dt | t <- [a+dt/2, a+3*dt/2 .. b-dt/2]]
