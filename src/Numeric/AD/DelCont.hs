{-| Reverse-mode automatic differentiation using delimited continuations

== References

* Wang et al, Backpropagation with Continuation Callbacks:Foundations for Efficient and ExpressiveDifferentiable Programming, NeurIPS 2018 https://papers.nips.cc/paper/2018/file/34e157766f31db3d2099831d348a7933-Paper.pdf

* Wang et al, Demystifying Differentiable Programming:Shift/Reset the Penultimate Backpropagator, ICFP 2019 https://www.cs.purdue.edu/homes/rompf/papers/wang-icfp19.pdf
-}
module Numeric.AD.DelCont (rad1, rad2
                          -- * Advanced
                          , rad1g, rad2g, op1, op2
                          -- * Types
                          , AD, AD') where

import Numeric.AD.DelCont.Internal (rad1, rad2, rad1g, rad2g, op1, op2, AD, AD')

