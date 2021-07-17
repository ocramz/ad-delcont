{-| Reverse-mode automatic differentiation using delimited continuations

== Quickstart



== Advanced use

== Implementation

The interface is inspired by that of @ad@ and @backprop@, but the internals are completely different in that here we don't rely on reifying the user function into a Wengert "tape" data structure.

This is the first (known) Haskell implementation of the ideas presented in Wang et al. Here the role of variable mutation and delimited continuations is made explicit by the use of 'ST' and 'ContT'.

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

