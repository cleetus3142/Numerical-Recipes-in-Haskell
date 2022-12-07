-- Copied and modified from https://rosettacode.org/wiki/Gaussian_elimination#Haskell
{-  To use this program do:

$ ghci
GHCi, version 8.8.4: https://www.haskell.org/ghc/  :? for help
Loaded package environment from /home/cleetus/.ghc/x86_64-linux-8.8.4/environments/default
Prelude> -- I load a program and create variables containing arrays
Prelude> :load gaussElim.hs
[1 of 1] Compiling Main             ( gaussElim.hs, interpreted )
Ok, one module loaded.
*Main> aa=[[2.0,5.0],[3.0,7.0]]
*Main> bb=[[1.0],[3.0]]
*Main> -- That is the A*x = b in matrix form
*Main> -- Now solve for x using Gaussian elimination
*Main> x = gauss aa bb
*Main> x
[[8.0],[-3.0]]
*Main> -- x is there
*Main> aa
[[2.0,5.0],[3.0,7.0]]
*Main> bb
[[1.0],[3.0]]
*Main> -- so is aa and bb
*Main> :load matrixMultiply.hs
[1 of 1] Compiling Main             ( matrixMultiply.hs, interpreted )
Ok, one module loaded.
*Main> aa

<interactive>:14:1: error: Variable not in scope: aa
*Main> bb

<interactive>:15:1: error: Variable not in scope: bb
*Main> -- Ack, after loading the new program the variables are erased
*Main> -- Loading matrixMultiply.hs wiped out aa, bb and x. Recopy them
*Main> aa=[[2.0,5.0],[3.0,7.0]]
*Main> bb=[[1.0],[3.0]]
*Main> x=[[8.0],[-3.0]]
*Main> -- Now check your answer with matrixMultiply.hs
*Main> mmult aa x
[[1.0],[3.0]]
*Main> bb
[[1.0],[3.0]]
*Main> -- Perfect!
*Main> 
-}

isMatrix xs = null xs || all ((== (length.head $ xs)).length) xs

isSquareMatrix xs = null xs || all ((== (length xs)).length) xs

mult:: (Num a) => [[a]] -> [[a]] -> [[a]]
mult uss vss = map ((\xs -> if null xs then [] else foldl1 (zipWith (+)) xs). zipWith (\vs u -> map (u*) vs) vss) uss

gauss::[[Double]] -> [[Double]] -> [[Double]]
gauss xs bs = map (map fromRational) $ solveGauss (toR xs) (toR bs)
    where toR = map $ map toRational

solveGauss:: (Fractional a, Ord a) => [[a]] -> [[a]] -> [[a]]
solveGauss xs bs | null xs || null bs || length xs /= length bs || (not $ isSquareMatrix xs) || (not $ isMatrix bs) = []
                 | otherwise = uncurry solveTriangle $ triangle xs bs

solveTriangle::(Fractional a,Eq a) => [[a]] -> [[a]] -> [[a]]
solveTriangle us _ | not.null.dropWhile ((/= 0).head) $ us = []
solveTriangle ([c]:as) (b:bs) = go as bs [map (/c) b]
  where
  val us vs ws = let u = head us in map (/u) $ zipWith (-) vs (head $ mult [tail us] ws)
  go [] _ zs          = zs
  go _ [] zs          = zs
  go (x:xs) (y:ys) zs = go xs ys $ (val x y zs):zs

triangle::(Num a, Ord a) => [[a]] -> [[a]] -> ([[a]],[[a]])
triangle xs bs = triang ([],[]) (xs,bs)
    where
    triang ts (_,[]) = ts
    triang ts ([],_) = ts
    triang (os,ps) zs = triang (us:os,cs:ps).unzip $ [(fun tus vs, fun cs es) | (v:vs,es) <- zip uss css,let fun = zipWith (\x y -> v*x - u*y)]
        where ((us@(u:tus)):uss,cs:css) = bubble zs

bubble::(Num a, Ord a) => ([[a]],[[a]]) -> ([[a]],[[a]])
bubble (xs,bs) = (go xs, go bs)
    where
    idmax = snd.maximum.flip zip [0..].map (abs.head) $ xs
    go ys = let (us,vs) = splitAt idmax ys in vs ++ us
    
