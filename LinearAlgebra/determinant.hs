-- Imported and modified from
-- https://rosettacode.org/wiki/Cramer%27s_rule#Haskell

{- To use this program do:
$ ghci
Loaded package environment from /home/cleetus/.ghc/x86_64-openbsd-9.2.4/environments/default
GHCi, version 9.2.4: https://www.haskell.org/ghc/  :? for help
ghci> :l determinant.hs
[1 of 1] Compiling Main             ( determinant.hs, interpreted )
Ok, one module loaded.
ghci> a = [[1,2,3],[4,5,6],[7,8,9]]
ghci> det a
0
ghci> b = [[1,1,1],[0,5,6],[0,0,9]]
ghci> det b
45
ghci>
-}

outer :: (a->b->c) -> [a] -> [b] -> [[c]]
outer f [] _       = []
outer f _ []       = []
outer f (h1:t1) x2 = (f h1 <$> x2) : outer f t1 x2

dot [] []           = 0
dot (h1:t1) (h2:t2) = (h1*h2) + (dot t1 t2)

transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss)
  = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

mul :: Num a => [[a]] -> [[a]] -> [[a]]
mul a b = outer dot a (transpose b)

delRow :: Int -> [a] -> [a]
delRow i v =
  (first ++ rest) where (first, _:rest) = splitAt i v

delCol :: Int -> [[a]] -> [[a]]
delCol j m = (delRow j) <$> m

-- Determinant:
adj :: Num a => [[a]] -> [[a]]
adj [] = []
adj m =
  [
    [(-1)^(i+j) * det (delRow i $ delCol j m)
    | i <- [0.. -1+length m]
    ]
  | j <- [0.. -1+length m]
  ]
det :: Num a => [[a]] -> a
det [] = 1
det m  = (mul m (adj m)) !! 0 !! 0
