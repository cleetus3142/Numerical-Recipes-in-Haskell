-- Computes a double integral by the trapezoid rule
{-

   /b /d
   |  |
   |  | f(x,y) dydx
   |  |
   /a /c
or in polar coordinates

   /R /2*pi
   |  |
   |  | f(r,q) r*dqdr
   |  |
   /0 /0



NOTE: This routine only works for constant limits of integration. 

The derivation of the routine:
   _ _ _ _ _ _
  |a|_|_|_|_|a|
  |_|_|_|_|_|_|
  |_|_|_|_|_|_|
  |_|_|_|_|_|_|
  |_|_|_|_|_|_|  
  |a|_|_|_|_|a|

The corners (a) are given by:
dt^2*(f lft lwr  + f rght lwr  + f lft uper + f rght uper)/4.0
The bottom row between the (a) is give by
(sum [ (f x lwr)*dt^2 | x <- [lft+dt,lft+2*dt..rght-dt]])/2.0
The top row between the (a) is given by
(sum [ (f x uper)*dt^2 | x <- [lft+dt,lft+2*dt..rght-dt]])/2.0
The left column between the (a) is given by
(sum [ (f lft y)*dt^2 | y <- [lwr+dt, lwr+2*dt..uper-dt]])/2.0
The right column between the (a) is given by
(sum [ (f rght y)*dt^2 | y <- [lwr+dt, lwr+2*dt..uper-dt]])/2.0
Finally, the interior area is given by:
sum [ (f x y)*dt^2 | x <- [lft+dt,lft+2*dt..rght-dt], y <- [lwr+dt, lwr+2*dt..uper-dt]]
Adding these all up you get your integral.
-}
-- BSD 2-Clause License Copyright -- (c) 2022, Jonathan Drews

type R=Double

dblIntg ::  R -> (R -> R -> R) -> R -> R -> R -> R -> R 
dblIntg dt f lft rght lwr uper  = dt^2*(f lft lwr  + f rght lwr  + f lft uper + f rght uper)/4.0 +
  (sum [ (f x lwr)*dt^2 | x <- [lft+dt,lft+2*dt..rght-dt]])/2.0 +
  (sum [ (f x uper)*dt^2 | x <- [lft+dt,lft+2*dt..rght-dt]])/2.0 +
  (sum [ (f lft y)*dt^2 | y <- [lwr+dt, lwr+2*dt..uper-dt]])/2.0 +
  (sum [ (f rght y)*dt^2 | y <- [lwr+dt, lwr+2*dt..uper-dt]])/2.0 +
  sum [ (f x y)*dt^2 | x <- [lft+dt,lft+2*dt..rght-dt], y <- [lwr+dt, lwr+2*dt..uper-dt]]
  
