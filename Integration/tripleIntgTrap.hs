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

-- Try to use Float instead of Double as Double precision really
-- makes for long run times.

type R=Double

trplIntg ::  Int -> (R -> R -> R -> R) -> R -> R -> R -> R -> R -> R -> R 
trplIntg n f x1 x2 y1 y2 z1 z2  =
  let dx=(x2-x1)/fromIntegral(2*n)
      dy=(y2-y1)/fromIntegral(2*n)
      dz=(z2-z1)/fromIntegral(2*n)
  in dx*dy*dz*(f x2 y1 z1  + f x2 y2 z1  + f x2 y1 z2 + f x2 y2 z2  +
  f x1 y1 z1  + f x1 y2 z1  + f x1 y1 z2 + f x1 y2 z2)/8.0 +         -- 8 corners
  ((sum [ (f x y1 z1)*dx*dy*dz | x <- [x1+dx, x1+2*dx..x2-dx]]) +
  (sum [ (f x y1 z2)*dx*dy*dz | x <- [x1+dx, x1+2*dx..x2-dx]]) +
  (sum [ (f x y2 z1)*dx*dy*dz | x <- [x1+dx, x1+2*dx..x2-dx]]) +
  (sum [ (f x y2 z2)*dx*dy*dz | x <- [x1+dx, x1+2*dx..x2-dx]]) +
  (sum [ (f x1 y z1)*dx*dy*dz | y <- [y1+dy, y1+2*dy..y2-dy]]) +
  (sum [ (f x2 y z1)*dx*dy*dz | y <- [y1+dy, y1+2*dy..y2-dy]]) +
  (sum [ (f x1 y z2)*dx*dy*dz | y <- [y1+dy, y1+2*dy..y2-dy]]) +  -- 12edges
  (sum [ (f x2 y z2)*dx*dy*dz | y <- [y1+dy, y1+2*dy..y2-dy]]) +
  (sum [ (f x1 y1 z)*dx*dy*dz | z <- [z1+dz, z1+2*dz..z2-dz]]) +
  (sum [ (f x2 y1 z)*dx*dy*dz | z <- [z1+dz, z1+2*dz..z2-dz]]) +
  (sum [ (f x1 y2 z)*dx*dy*dz | z <- [z1+dz, z1+2*dz..z2-dz]]) +
  (sum [ (f x2 y2 z)*dx*dy*dz | z <- [z1+dz, z1+2*dz..z2-dz]]))/4.0 +
  ((sum [ (f x y z1)*dx*dy*dz | x <- [x1+dx, x1+2*dx..x2-dx], y <- [y1+dy, y1+2*dy..y2-dy]])+
  (sum [ (f x y z2)*dx*dy*dz | x <- [x1+dx, x1+2*dx..x2-dx], y <- [y1+dy, y1+2*dy..y2-dy]]) +
  (sum [ (f x1 y z)*dx*dy*dz | y <- [y1+dy, y1+2*dy..y2-dy], z <- [z1+dz, z1+2*dz..z2-dz]]) +  -- 6 faces 
  (sum [ (f x2 y z)*dx*dy*dz | y <- [y1+dy, y1+2*dy..y2-dy], z <- [z1+dz, z1+2*dz..z2-dz]]) +
  (sum [ (f x y1 z)*dx*dy*dz | x <- [x1+dx, x1+2*dx..x2-dx], z <- [z1+dz, z1+2*dz..z2-dz]]) +
  (sum [ (f x y2 z)*dx*dy*dz | x <- [x1+dx, x1+2*dx..x2-dx], z <- [z1+dz, z1+2*dz..z2-dz]]))/2.0 +    -- The interior
  sum [ (f x y z)*dx*dy*dz | x <- [x1+dx,x1+2*dx..x2-dx], y <- [y1+dy, y1+2*dy..y2-dy],
  z <- [z1+dz, z1+2*dz..z2-dz]]
  
