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
dt^2*(f x1 y1  + f x2 y1  + f x1 y2 + f x2 y2)/4.0
The bottom row between the (a) is give by
(sum [ (f x y1)*dt^2 | x <- [x1+dt,x1+2*dt..x2-dt]])/2.0
The top row between the (a) is given by
(sum [ (f x y2)*dt^2 | x <- [x1+dt,x1+2*dt..x2-dt]])/2.0
The left column between the (a) is given by
(sum [ (f x1 y)*dt^2 | y <- [y1+dt, y1+2*dt..y2-dt]])/2.0
The right column between the (a) is given by
(sum [ (f x2 y)*dt^2 | y <- [y1+dt, y1+2*dt..y2-dt]])/2.0
Finally, the interior area is given by:
sum [ (f x y)*dt^2 | x <- [x1+dt,x1+2*dt..x2-dt], y <- [y1+dt, y1+2*dt..y2-dt]]
Adding these all up you get your integral.
-}
-- BSD 2-Clause License Copyright -- (c) 2022, Jonathan Drews

type R=Float


dblIntg ::  R -> (R -> R -> R) -> R -> R -> R -> R -> R 
dblIntg dt f x1 x2 y1 y2  = dt^2*(f x1 y1  + f x2 y1  + f x1 y2 + f x2 y2)/4.0 +
  (sum [ (f x y1)*dt^2 | x <- [x1+dt,x1+2*dt..x2-dt]])/2.0 +
  (sum [ (f x y2)*dt^2 | x <- [x1+dt,x1+2*dt..x2-dt]])/2.0 +
  (sum [ (f x1 y)*dt^2 | y <- [y1+dt, y1+2*dt..y2-dt]])/2.0 +
  (sum [ (f x2 y)*dt^2 | y <- [y1+dt, y1+2*dt..y2-dt]])/2.0 +
  sum [ (f x y)*dt^2 | x <- [x1+dt,x1+2*dt..x2-dt], y <- [y1+dt, y1+2*dt..y2-dt]]
  
