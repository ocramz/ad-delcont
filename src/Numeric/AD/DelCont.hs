{-| Reverse-mode automatic differentiation using delimited continuations

== Quickstart

Most users will only need to import 'rad1' , 'rad2' and be aware that 'AD' has a 'Num' instance.

Similarly to @ad@, a user codes the function to be differentiated against the Num typeclass, e.g.

@
f :: Num a => a -> a
f = \\x -> x + (x * x)
@

and the library takes care of the rest :

@
>>> 'rad1' f 1.2
(2.6399999999999997,3.4000000000000004)
@

== Advanced use

The library is small and easily extensible. For example, a user might want to supply their own numerical typeclass other than 'Num', and build up a library of 'AD' combinators based on that, by using 'op1' and 'op2'.

== Implementation

This is the first (known) Haskell implementation of the ideas presented in Wang et al. Here the role of variable mutation and delimited continuations is made explicit by the use of 'ST' and 'ContT', as compared to the reference Scala implementation. 

The interface is inspired by that of @ad@ and @backprop@, however the internals are completely different in that this library doesn't rely on reifying the user function into a Wengert "tape" data structure.


== References

* Wang et al, Backpropagation with Continuation Callbacks : Foundations for Efficient and ExpressiveDifferentiable Programming, NeurIPS 2018 - https://papers.nips.cc/paper/2018/file/34e157766f31db3d2099831d348a7933-Paper.pdf

* Wang et al, Demystifying Differentiable Programming : Shift\/Reset the Penultimate Backpropagator, ICFP 2019 - https://www.cs.purdue.edu/homes/rompf/papers/wang-icfp19.pdf
-}
module Numeric.AD.DelCont (rad1, rad2
                          -- * Advanced
                          , rad1g, rad2g,
                            -- ** Lift operators into AD
                            op1ad, op2ad
                            -- *** ContT internals
                          , op1, op2
                          -- * Types
                          , AD, AD') where

import Numeric.AD.DelCont.Internal (rad1, rad2, rad1g, rad2g, op1ad, op2ad, op1, op2, AD, AD')

