-- Computes a triple integral by the trapezoid rule
{-

   /b /d  /f
   |  |  |
   |  |  | f(x,y,z) dzdydx
   |  |  |
   /a /c /e
or in cylindrical coordinates

   /Z /R  /2*pi
   |  |  |
   |  |  | f(r,q,z) r*dqdrdz
   |  |  |
   /0 /0 /0



NOTE: This routine only works for constant limits of integration.

-}
-- BSD 2-Clause License Copyright -- (c) 2022, Jonathan Drews

-- Try to use Float instead of Double as Double	precision really
-- makes for long run times.

type R=Double

trplIntg ::  R -> (R -> R -> R -> R) -> R -> R -> R -> R -> R -> R -> R
trplIntg dt f x1 x2 y1 y2 z1 z2  =
  dt^3*(f x2 y1 z1  + f x2 y2 z1  + f x2 y1 z2 + f x2 y2 z2  +
  f x1 y1 z1  + f x1 y2 z1  + f x1 y1 z2 + f x1 y2 z2)/8.0 +
  ((sum [ (f x y1 z1)*dt^3 | x <- [x1+dt, x1+2*dt..x2-dt]]) +
  (sum [ (f x y1 z2)*dt^3 | x <- [x1+dt, x1+2*dt..x2-dt]]) +
  (sum [ (f x y2 z1)*dt^3 | x <- [x1+dt, x1+2*dt..x2-dt]]) +
  (sum [ (f x y2 z2)*dt^3 | x <- [x1+dt, x1+2*dt..x2-dt]]) +
  (sum [ (f x1 y z1)*dt^3 | y <- [y1+dt, y1+2*dt..y2-dt]]) +
  (sum [ (f x2 y z1)*dt^3 | y <- [y1+dt, y1+2*dt..y2-dt]]) +
  (sum [ (f x1 y z2)*dt^3 | y <- [y1+dt, y1+2*dt..y2-dt]]) +
  (sum [ (f x2 y z2)*dt^3 | y <- [y1+dt, y1+2*dt..y2-dt]]) +
  (sum [ (f x1 y1 z)*dt^3 | z <- [z1+dt, z1+2*dt..z2-dt]]) +
  (sum [ (f x2 y1 z)*dt^3 | z <- [z1+dt, z1+2*dt..z2-dt]]) +
  (sum [ (f x1 y2 z)*dt^3 | z <- [z1+dt, z1+2*dt..z2-dt]]) +
  (sum [ (f x2 y2 z)*dt^3 | z <- [z1+dt, z1+2*dt..z2-dt]]))/4.0 +
  ((sum [ (f x y z1)*dt^3 | x <- [x1+dt, x1+2*dt..x2-dt], y <- [y1+dt, y1+2*dt..y2-dt]]) +
  (sum [ (f x y z2)*dt^3 | x <- [x1+dt, x1+2*dt..x2-dt], y <- [y1+dt, y1+2*dt..y2-dt]]) +
  (sum [ (f x1 y z)*dt^3 | y <- [y1+dt, y1+2*dt..y2-dt], z <- [z1+dt, z1+2*dt..z2-dt]]) +
  (sum [ (f x2 y z)*dt^3 | y <- [y1+dt, y1+2*dt..y2-dt], z <- [z1+dt, z1+2*dt..z2-dt]]) +
  (sum [ (f x y1 z)*dt^3 | x <- [x1+dt, x1+2*dt..x2-dt], z <- [z1+dt, z1+2*dt..z2-dt]]) +
  (sum [ (f x y2 z)*dt^3 | x <- [x1+dt, x1+2*dt..x2-dt], z <- [z1+dt, z1+2*dt..z2-dt]]))/2.0 +
  sum [ (f x y z)*dt^3 | x <- [x1+dt,x1+2*dt..x2-dt], y <- [y1+dt, y1+2*dt..y2-dt],
  z <- [z1+dt, z1+2*dt..z2-dt]]
  
