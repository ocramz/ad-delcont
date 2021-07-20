{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
module Numeric.AD.DelCont.Internal
  (rad1, rad2,
   auto,
   rad1g, rad2g,
   op1Num, op2Num,
   op1, op2,
   AD, AD')
  where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (Bifunctor(..))
import Data.Proxy (Proxy (..))
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')

-- transformers
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Cont (ContT, shiftT, resetT, evalContT)

import Prelude hiding (read)


-- | Dual numbers
data D a da = D a da deriving (Show, Functor)
instance Eq a => Eq (D a da) where
  D x _ == D y _ = x == y
instance Ord a => Ord (D a db) where
  compare (D x _) (D y _) = compare x y
instance Bifunctor D where
  bimap f g (D a b) = D (f a) (g b)

-- | Modify the adjoint part of a 'D'
withD :: (da -> db) -> D a da -> D a db
withD = second

-- | Differentiable variable
--
-- A (safely) mutable reference to a dual number
type DVar s a da = STRef s (D a da)
-- | Introduce a fresh DVar
var :: a -> da -> ST s (DVar s a da)
var x dx = newSTRef (D x dx)

-- | Lift a constant value into 'AD'
--
-- As one expects from a constant, its value will be used for computing the result, but it will be discarded when computing the sensitivities.
auto :: a -> AD s a da
auto x = AD $ lift $ var x undefined


-- | Mutable references to dual numbers in the continuation monad
--
-- Here the @a@ and @da@ type parameters are respectively the /primal/ and /dual/ quantities tracked by the AD computation.
--
-- The current implementation relies on 'forall x . ContT x ...', which is elsewhere called Codensity. Not sure of the implications of this
newtype AD s a da = AD { unAD :: forall x . ContT x (ST s) (DVar s a da) }
-- | Like 'AD' but the types of primal and dual coincide
type AD' s a = AD s a a

-- runAD :: (forall s . AD s a da) -> D a da
-- runAD go = runST (evalContT  (unAD go) >>= readSTRef)

-- | Lift a unary function
--
-- This is a polymorphic combinator for tracking how primal and adjoint values are transformed by a function.
--
-- How does this work :
--
-- 1) Compute the function result and bind the function inputs to the adjoint updating function (the "pullback")
--
-- 2) Allocate a fresh STRef @rb@ with the function result and @zero@ adjoint part
--
-- 3) @rb@ is passed downstream as an argument to the continuation @k@, with the expectation that the STRef will be mutated
--
-- 4) Upon returning from the @k@ (bouncing from the boundary of @resetT@), the mutated STRef is read back in
--
-- 5) The adjoint part of the input variable is updated using @rb@ and the result of the continuation is returned.
op1_ :: db -- ^ zero
     -> (da -> da -> da) -- ^ plus
     -> (a -> (b, db -> da)) -- ^ returns : (function result, pullback)
     -> ContT x (ST s) (DVar s a da)
     -> ContT x (ST s) (DVar s b db)
op1_ zeroa plusa f ioa = do
  ra <- ioa
  (D xa _) <- lift $ readSTRef ra
  let (xb, g) = f xa -- 1)
  shiftT $ \ k -> lift $ do
    rb <- var xb zeroa -- 2)
    ry <- k rb -- 3)
    (D _ yd) <- readSTRef rb -- 4)
    modifySTRef' ra (withD (\rda0 -> rda0 `plusa` g yd)) -- 5)
    pure ry


-- | Lift a unary function
--
-- The first two arguments constrain the types of the adjoint values of the output and input variable respectively, see 'op1Num' for an example.
--
-- The third argument is the most interesting: it specifies at once how to compute the function value and how to compute the sensitivity with respect to the function parameter.
--
-- Note : the type parameters are completely unconstrained.
op1 :: db -- ^ zero
    -> (da -> da -> da) -- ^ plus
    -> (a -> (b, db -> da)) -- ^ returns : (function result, pullback)
    -> AD s a da
    -> AD s b db
op1 z plusa f (AD ioa) = AD $ op1_ z plusa f ioa

-- | Helper for constructing unary functions that operate on Num instances (i.e. 'op1' specialized to Num)
op1Num :: (Num da, Num db) =>
          (a -> (b, db -> da)) -- ^ returns : (function result, pullback)
       -> AD s a da
       -> AD s b db
op1Num = op1 0 (+)

-- | Lift a binary function
op2_ :: dc -- ^ zero
     -> (da -> da -> da) -- ^ plus
     -> (db -> db -> db) -- ^ plus
     -> (a -> b -> (c, dc -> da, dc -> db)) -- ^ returns : (function result, pullbacks)
     -> ContT x (ST s) (DVar s a da)
     -> ContT x (ST s) (DVar s b db)
     -> ContT x (ST s) (DVar s c dc)
op2_ zeroa plusa plusb f ioa iob = do
  ra <- ioa
  rb <- iob
  (D xa _) <- lift $ readSTRef ra
  (D xb _) <- lift $ readSTRef rb
  let (xc, g, h) = f xa xb
  shiftT $ \ k -> lift $ do
    rc <- var xc zeroa
    ry <- k rc
    (D _ yd) <- readSTRef rc
    modifySTRef' ra (withD (\rda0 -> rda0 `plusa` g yd))
    modifySTRef' rb (withD (\rdb0 -> rdb0 `plusb` h yd))
    pure ry

-- | Lift a binary function
--
-- See 'op1' for more information.
op2 :: dc -- ^ zero
    -> (da -> da -> da) -- ^ plus
    -> (db -> db -> db) -- ^ plus
    -> (a -> b -> (c, dc -> da, dc -> db)) -- ^ returns : (function result, pullbacks)
    -> (AD s a da -> AD s b db -> AD s c dc)
op2 z plusa plusb f (AD ioa) (AD iob) = AD $ op2_ z plusa plusb f ioa iob

-- | Helper for constructing binary functions that operate on Num instances (i.e. 'op2' specialized to Num)
op2Num :: (Num da, Num db, Num dc) =>
          (a -> b -> (c, dc -> da, dc -> db)) -- ^ returns : (function result, pullback)
       -> AD s a da
       -> AD s b db
       -> AD s c dc
op2Num = op2 0 (+) (+)

-- | The numerical methods of (Num, Fractional, Floating etc.) can be read off their @backprop@ counterparts : https://hackage.haskell.org/package/backprop-0.2.6.4/docs/src/Numeric.Backprop.Op.html#%2A.
instance (Num a) => Num (AD s a a) where
  (+) = op2Num $ \x y -> (x + y, id, id)
  (-) = op2Num $ \x y -> (x - y, id, negate)
  (*) = op2Num $ \x y -> (x*y, (y *), (x *))
  fromInteger x = auto (fromInteger x)
  abs = op1Num $ \x -> (abs x, (* signum x))
  signum = op1Num $ \x -> (signum x, const 0)

instance (Fractional a) => Fractional (AD s a a) where
  (/) = op2Num $ \x y -> (x / y, (/ y), (\g -> -g*x/(y*y) ))
  fromRational x = auto (fromRational x)
  recip = op1Num $ \x -> (recip x, (/(x*x)) . negate)

instance Floating a => Floating (AD s a a) where
  pi = auto pi
  exp = op1Num $ \x -> (exp x, (exp x *))
  log = op1Num $ \x -> (log x, (/x))
  sqrt = op1Num $ \x -> (sqrt x, (/ (2 * sqrt x)))
  logBase = op2Num $ \x y ->
                       let
                         dx = - logBase x y / (log x * x)
                       in ( logBase x y
                          , ( * dx)
                          , (/(y * log x))
                          )
  sin = op1Num $ \x -> (sin x, (* cos x))
  cos = op1Num $ \x -> (cos x, (* (-sin x)))
  tan = op1Num $ \x -> (tan x, (/ cos x^(2::Int)))
  asin = op1Num $ \x -> (asin x, (/ sqrt(1 - x*x)))
  acos = op1Num $ \x -> (acos x, (/ sqrt (1 - x*x)) . negate)
  atan = op1Num $ \x -> (atan x, (/ (x*x + 1)))
  sinh = op1Num $ \x -> (sinh x, (* cosh x))
  cosh = op1Num $ \x -> (cosh x, (* sinh x))
  tanh = op1Num $ \x -> (tanh x, (/ cosh x^(2::Int)))
  asinh = op1Num $ \x -> (asinh x, (/ sqrt (x*x + 1)))
  acosh = op1Num $ \x -> (acosh x, (/ sqrt (x*x - 1)))
  atanh = op1Num $ \x -> (atanh x, (/ (1 - x*x)))

-- instance Eq a => Eq (AD s a da) where -- ??? likely impossible
-- instance Ord (AD s a da) where -- ??? see above



-- | Evaluate (forward mode) and differentiate (reverse mode) a unary function, without committing to a specific numeric typeclass
rad1g :: da -- ^ zero
      -> db -- ^ one
      -> (forall s . AD s a da -> AD s b db)
      -> a -- ^ function argument
      -> (b, da) -- ^ (result, adjoint)
rad1g zeroa oneb f x = runST $ do
  xr <- var x zeroa
  zr' <- evalContT $
    resetT $ do
      let
        z = f (AD (pure xr))
      zr <- unAD z
      lift $ modifySTRef' zr (withD (const oneb))
      pure zr
  (D z _) <- readSTRef zr'
  (D _ x_bar) <- readSTRef xr
  pure (z, x_bar)



-- | Evaluate (forward mode) and differentiate (reverse mode) a binary function, without committing to a specific numeric typeclass
rad2g :: da -- ^ zero
      -> db -- ^ zero
      -> dc -- ^ one
      -> (forall s . AD s a da -> AD s b db -> AD s c dc)
      -> a -> b
      -> (c, (da, db)) -- ^ (result, adjoints)
rad2g zeroa zerob onec f x y = runST $ do
  xr <- var x zeroa
  yr <- var y zerob
  zr' <- evalContT $
    resetT $ do
      let
        z = f (AD (pure xr)) (AD (pure yr))
      zr <- unAD z
      lift $ modifySTRef' zr (withD (const onec))
      pure zr
  (D z _) <- readSTRef zr'
  (D _ x_bar) <- readSTRef xr
  (D _ y_bar) <- readSTRef yr
  pure (z, (x_bar, y_bar))


-- | Evaluate (forward mode) and differentiate (reverse mode) a unary function
--
-- >>> rad1 (\x -> x * x) 1
-- (1, 2)
rad1 :: (Num a, Num b) =>
        (forall s . AD' s a -> AD' s b) -- ^ function to be differentiated
     -> a -- ^ function argument
     -> (b, a) -- ^ (result, adjoint)
rad1 = rad1g 0 1

-- | Evaluate (forward mode) and differentiate (reverse mode) a binary function
--
-- >>> rad2 (\x y -> x + y + y) 1 1
-- (1,2)
--
-- >>> rad2 (\x y -> (x + y) * x) 3 2
-- (15,(8,3))
rad2 :: (Num a, Num b, Num c) =>
        (forall s . AD' s a -> AD' s b -> AD' s c) -- ^ function to be differentiated
     -> a
     -> b
     -> (c, (a, b)) -- ^ (result, adjoints)
rad2 = rad2g 0 0 1



-- ======================== EXPERIMENTAL ==========================

data Backprop a da = Backprop {
  zero :: a -> da
  , one :: da -> da
  , add :: da -> da -> da
                              }

bpNum :: (Num a, Num da) => Backprop a da
bpNum = Backprop zeroNum oneNum addNum

-- | backprop typeclass, adapted from https://hackage.haskell.org/package/backprop-0.2.6.4/docs/src/Numeric.Backprop.Class.html
--
-- we use two type parameters to keep the distinction between primal and dual variables

-- class Backprop a da where
--   zero :: a -> da
--   one :: proxy a -> da -> da
--   add :: proxy a -> da -> da -> da

-- | 'zero' for instances of 'Num'. lazy in its argument.
zeroNum :: Num da => a -> da
zeroNum _ = 0
{-# INLINE zeroNum #-}

-- | 'add' for instances of 'Num'.
addNum :: Num da => da -> da -> da
addNum = (+)
{-# INLINE addNum #-}

-- | 'one' for instances of 'Num'. lazy in its argument.
oneNum :: Num da => a -> da
oneNum _ = 1
{-# INLINE oneNum #-}


rad1BP :: Backprop a da
       -> Backprop b db
       -> (forall s . AD s a da -> AD s b db)
       -> a -- ^ function argument
       -> (b, da) -- ^ (result, adjoint)
rad1BP bpa bpb f x = runST $ do
  xr <- var x (zero bpa x)
  zr' <- evalContT $
    resetT $ do
      let
        z = f (AD (pure xr))
      zr <- unAD z
      lift $ modifySTRef' zr (withD $ one bpb)
      pure zr
  (D z _) <- readSTRef zr'
  (D _ x_bar) <- readSTRef xr
  pure (z, x_bar)

-- rad1BP :: (Backprop a da, Backprop b db)
--        => (forall s . AD s a da -> AD s b db)
--        -> a -- ^ function argument
--        -> (b, da) -- ^ (result, adjoint)
-- rad1BP f x = runST $ do
--   xr <- var x (zero x)
--   zr' <- evalContT $
--     resetT $ do
--       let
--         z = f (AD (pure xr))
--       zr <- unAD z
--       let
--         oneB :: forall b db . Backprop b db => db -> db -> db
--         oneB = one (Proxy :: Proxy db)
--       lift $ modifySTRef' zr (withD oneB)
--       pure zr
--   (D z _) <- readSTRef zr'
--   (D _ x_bar) <- readSTRef xr
--   pure (z, x_bar)





-- -- playground

-- -- dual pairing 
-- class Dual a where
--   dual :: Num r => a -> (a -> r)

-- -- | Dual numbers DD (alternative take, using a type family for the first variation)
-- data DD a = Dd a (Adj a)
-- class Diff a where type Adj a :: *
-- instance Diff Double where type Adj Double = Double


-- -- product type (simplified version of vinyl's Rec)
-- data Rec :: [*] -> * where
--   RNil :: Rec '[]
--   (:*) :: !a -> !(Rec as) -> Rec (a ': as)

-- data SDRec s as where
--   SDNil :: SDRec s '[]
--   (:&) :: DVar s a a -> !(SDRec s as) -> SDRec s (a ': as)
