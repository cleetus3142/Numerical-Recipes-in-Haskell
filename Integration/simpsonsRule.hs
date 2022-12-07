-- Computes the integral by simpsons rule: I ~ 4/3*dt*(f(a+dt) + f(a+3*dt) + ...
-- f(b-dt)) + 2/3*dt*(f(a+2*dt) + f(a+4*dt) + ... + f(b-2*dt)) +
-- dt/3*(f(a) + f(b))
{-
BSD 2-Clause License

Copyright (c) 2022, Jonathan Drews

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

    Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation 
    and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-}

sqrd :: Double -> Double
sqrd x = x^2

type R = Double
type Integration = (R -> R) -> R -> R -> R

integral :: R -> Integration
integral dt f a b = dt/3.0*(f(a)+f(b)) +
 4.0/3.0*(sum [ f t * dt | t <- [a+dt, a+3*dt..b-dt]]) +
 2.0/3.0*(sum [ f t * dt | t <- [a+2*dt, a+4*dt..b-2*dt]])
 
