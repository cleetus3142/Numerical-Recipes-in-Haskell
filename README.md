# Numerical-Recipes-in-Haskell
Here is a collection of simple Haskell programs that could be a help to people learning Haskell. I recommend the following books for learning Haskell:

* Learn You a Haskell for Great Good! by  Miran Lipovaca
* Learn Physics with Functional Programming by  Scott N. Walck

**Important:** You have to use ghci to run these programs. Do like so:
```
$ ghci                         
GHCi, version 9.2.4: https://www.haskell.org/ghc/  :? for help
ghci> :load simpsonsRule.hs
[1 of 1] Compiling Main             ( simpsonsRule.hs, interpreted )
Ok, one module loaded.
ghci> integral 0.1 cos 0 pi
-5.8431016703149555e-2
```
Some of the linear algebra routines require packages from
cabal. They can be installed on OpenBSD with the following commands.
Cabal: Common Architecture for Building Applications and Libraries

First, you have to have cabal-install installed on your OpenBSD
computer. as root do:

\# pkg_add cabal-install

Then in your user account do:

$ cabal update
$ cabal install matrix
$ cabal install --lib matrix

Now you can run the program cramersRule.hs. Do the following:
```
$ ghci
Loaded package environment from /home/cleetus/.ghc/x86_64-openbsd-9.2.4/environments/default
GHCi, version 9.2.4: https://www.haskell.org/ghc/  :? for help
ghci> :l cramersRule.hs
[1 of 1] Compiling Main             ( cramersRule.hs, interpreted )
Ok, one module loaded.
ghci> m = [[1,0,0],[0,2,0],[0,0,3]]
ghci> b = [[1],[1],[1]]
ghci> task m b
Just [1.0,0.5,0.3333333333333333]
ghci>
```





