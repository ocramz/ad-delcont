{-| Reverse-mode automatic differentiation using delimited continuations.

== Quickstart

Most users will only need to import 'rad1', 'rad2' and leverage the 'Num', 'Fractional', 'Floating' instances of the 'AD' type.

Similarly to @ad@, a user supplies a /polymorphic/ function to be differentiated, e.g.

@
f :: Num a => a -> a
f x = x + (x * x)
@

and the library takes care of the rest :

@
>>> 'rad1' f 1.2
(2.6399999999999997,3.4000000000000004)
@

It's important to emphasize that the library cannot differentiate functions of concrete types, e.g. @Double -> Double@. On the other hand, it's easy to experiment with other numerical interfaces that support one, zero and plus.

== Advanced usage

The library is small and easily extensible.

For example, a user might want to supply their own numerical typeclass other than 'Num', and build up a library of 'AD' combinators based on that, specializing 'op1' and 'op2' with custom implementations of @zero@, @one@ and @plus@. This insight first appeared in the user interface of @backprop@, as the Backprop typeclass.

Exposing unconstrained AD combinators lets users specialize this library to e.g. exotic number-like types or discrete data structures such as dictionaries, automata etc.

== Implementation details and design choices

This is the first (known) Haskell implementation of the ideas presented in Wang et al. Here the role of variable mutation and delimited continuations is made explicit by the use of 'ST' and 'ContT', as compared to the reference Scala implementation.

@ad-delcont@ relies on non-standard interpretation of the user-provided function; in order to compute the adjoint values (the /sensitivities/) of the function parameters, the function is first evaluated ("forwards"), while keeping track of continuation points, and all the intermediate adjoints are accumulated upon returning from the respective continuations ("backwards") via safe mutation in the ST monad.

As a result of this design, the main 'AD' type cannot be given 'Eq' and 'Ord' instances (since it's unclear how equality and ordering predicates would apply to continuations and state threads).

The user interface is inspired by that of @ad@ and @backprop@, however the internals are completely different in that this library doesn't reify the function to be differentiated into a "tape" data structure.

Another point in common with @backprop@ is that users can differentiate heterogeneous functions: the input and output types can be different. This makes it possible to differentiate functions of statically-typed vectors and matrices.


== References

* @backprop@ - https://hackage.haskell.org/package/backprop

* @ad@ - https://hackage.haskell.org/package/ad

* F. Wang et al, Backpropagation with Continuation Callbacks : Foundations for Efficient and Expressive Differentiable Programming, NeurIPS 2018 - https://papers.nips.cc/paper/2018/file/34e157766f31db3d2099831d348a7933-Paper.pdf

* F. Wang et al, Demystifying Differentiable Programming : Shift\/Reset the Penultimate Backpropagator, ICFP 2019 - https://doi.org/10.1145/3341700 - https://www.cs.purdue.edu/homes/rompf/papers/wang-icfp19.pdf

* M. Innes, Don't unroll adjoint: Differentiating SSA-Form Programs  https://arxiv.org/abs/1810.07951
-}
module Numeric.AD.DelCont (-- * Quickstart
                            rad1, rad2
                          , auto
                          -- * Advanced usage
                          , rad1g, rad2g,
                            -- ** Lift functions into AD
                            op1, op2
                            -- *** Num instances
                          -- * Types
                          , AD, AD') where

import Numeric.AD.DelCont.Internal (rad1, rad2, auto, rad1g, rad2g, op1, op2, AD, AD')

