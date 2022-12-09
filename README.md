# Numerical-Routines-in-Haskell
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
cabal. Cabal: Common Architecture for Building Applications and Libraries

First, you have to have cabal-install installed on your OpenBSD
or Linux computer. 

Once cabal-install is installed, do this in your user account:
```
$ cabal update

$ cabal install matrix

$ cabal install --lib matrix
```









