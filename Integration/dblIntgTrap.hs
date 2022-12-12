-- Computes a double integral by the trapezoid rule
-- Computes the double integral by the trapezoid rule
-- n is half the number of steps for the imterval of integraion.
-- See the book "Differential and Integral Calculus"
-- by Richard Courant,Volume 1, page 343
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
dx*dy*(f x1 y1  + f x2 y1  + f x1 y2 + f x2 y2)/4.0
The bottom row between the (a) is give by
(sum [ (f x y1)*dx*dy | x <- [x1+dx,x1+2*dx..x2-dx]])/2.0
The top row between the (a) is given by
(sum [ (f x y2)*dx*dy | x <- [x1+dx,x1+2*dx..x2-dx]])/2.0
The left column between the (a) is given by
(sum [ (f x1 y)*dx*dy | y <- [y1+dy, y1+2*dy..y2-dy]])/2.0
The right column between the (a) is given by
(sum [ (f x2 y)*dx*dy | y <- [y1+dy, y1+2*dy..y2-dy]])/2.0
Finally, the interior area is given by:
sum [ (f x y)*dx*dy | x <- [x1+dx,x1+2*dx..x2-dx], y <- [y1+dy, y1+2*dy..y2-dy]]
Adding these all up you get your integral.
-}
-- BSD 2-Clause License Copyright -- (c) 2022, Jonathan Drews

type R=Float


dblIntg ::  Int -> (R -> R -> R) -> R -> R -> R -> R -> R

-- n is the half step size, f is the function, x1, x2, y1 & y2
-- are the limits of integration.

dblIntg n f x1 x2 y1 y2  =
  let dx=(x2-x1)/fromIntegral(2*n)
      dy=(y2-y1)/fromIntegral(2*n)
  in dx*dy*(f x1 y1  + f x2 y1  + f x1 y2 + f x2 y2)/4.0 +
  (sum [ (f x y1)*dx*dy | x <- [x1+dx,x1+2*dx..x2-dx]])/2.0 +
  (sum [ (f x y2)*dx*dy | x <- [x1+dx,x1+2*dx..x2-dx]])/2.0 +
  (sum [ (f x1 y)*dx*dy | y <- [y1+dy, y1+2*dy..y2-dy]])/2.0 +
  (sum [ (f x2 y)*dx*dy | y <- [y1+dy, y1+2*dy..y2-dy]])/2.0 +
  sum [ (f x y)*dx*dy | x <- [x1+dx,x1+2*dx..x2-dx], y <- [y1+dy, y1+2*dy..y2-dy]]
  
