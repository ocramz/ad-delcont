{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Numeric.AD.DelCont where

import Control.Monad.ST (ST, runST)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (Foldable(..))
import Data.Functor (void)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')

-- transformers
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Cont (Cont, shift, reset, evalCont, ContT(..), shiftT, resetT, evalContT, runContT)

import Prelude hiding (read)

-- try this to understand what 'shift' and 'reset' do.
t0 :: IO ()
t0 = evalContT $ do
  let say = liftIO . putStrLn
  resetT $ do
    say "A"
    say "B"
    x <- shiftT $ \k -> do
      say "C"
      lift $ k 1
      y <- shiftT $ \k2 -> do
        say "D"
        lift $ k2 2
      say $ unwords ["k2 :", show y]
      lift $ k 3
    say $ unwords ["k :", show x]




-- product type (simplified version of vinyl's Rec)
data Rec :: [*] -> * where
  RNil :: Rec '[]
  (:*) :: !a -> !(Rec as) -> Rec (a ': as)

data SDRec s as where
  SDNil :: SDRec s '[]
  (:&) :: DVar s a a -> !(SDRec s as) -> SDRec s (a ': as)


-- -- simplified version of Op from backprop
-- --
-- -- rather than functions, here we need to keep track of one STRef per operand
-- newtype Op as a = Op { runOpWith :: Rec as -> (a, a -> Rec as)}

-- op1 :: (a -> (b, b -> a))
--     -> Op '[a] b
-- op1 f = Op $ \(x :* RNil) -> let (y, dx) = f x
--                              in (y, \(!d) -> (dx d :* RNil))

-- differentiable variable
type DVar s a da = STRef s (D a da)

-- | Introduce a fresh DVar
var :: Num da => a -> ST s (DVar s a da)
var x = newSTRef (D x 0)

-- | Dual numbers
data D a da = D a da deriving (Show, Functor)




-- instance Applicative (D a) where -- need (Monoid a) -- (?)
-- --   (D a f) <*> (D b x) =
instance Bifunctor D where
  bimap f g (D a b) = D (f a) (g b)

withX :: (a -> b) -> D a da -> D b da
withX = first
withD :: (da -> db) -> D a da -> D a db
withD = second

instance Eq a => Eq (D a da) where
  D x _ == D y _ = x == y
instance Ord a => Ord (D a db) where
  compare (D x _) (D y _) = compare x y

-- type AD s r a = ContT r (ST s) a -- (Functor, Applicative, Monad)
-- evalAD :: (forall s . AD s a a) -> a
-- evalAD go = runST (evalContT go)

type AD s r a da = ContT r (ST s) (DVar s a da)

runAD :: (forall s . AD s (DVar s a da) a da) -> D a da
runAD go = runST $ runContT go pure >>= readSTRef





{-| https://papers.nips.cc/paper/2018/file/34e157766f31db3d2099831d348a7933-Paper.pdf
https://www.cs.purdue.edu/homes/rompf/papers/wang-icfp19.pdf

class NumR(val x: Double, var d: Double) {

  def + (that: NumR) = shift {(k : NumR => Unit) =>
    val y = new NumR(x + that.x, 0.0);
    k(y);
    this.d += y.d;
    that.d += y.d}

  def * (that: NumR) = shift {(k : NumR => Unit)=>
    val y = new NumR(x * that.x, 0.0);
    k(y)
    this.d += that.x * y.d;
    that.d += this.x * y.d
  }}

def grad(f: NumR => NumR )(x: Double) = {
  val z = new NumR(x, 0.0)
  reset { f(z).d = 1.0 }
  z.d
  }
-}

-- | An alternative implementation to unOp
--
-- here we pass DVar's around
--
-- HOW DOES THIS WORK :
--
-- 1) compute function result and adjoint update
-- 2) fresh STRef rb with result and 0 adjoint part
-- 3) rb will be passed downstream by the continuation k, with the expectation that the STRef will be mutated
-- 4) upon returning from k (bouncing from the boundary of resetT), the mutated STRef is read back in
-- 5) adjoint part of the input variable is updated and new input variable is returned.

op1 :: Num db =>
       (a -> (b, db -> da -> da))
    -> AD s r a da
    -> AD s r b db
op1 f ioa = do
  ra <- ioa
  (D xa _) <- lift $ readSTRef ra
  let (xb, g) = f xa -- 1)
  shiftT $ \ k -> lift $ do
    rb <- var xb -- 2)
    ry <- k rb -- 3)
    (D _ yd) <- readSTRef rb -- 4)
    modifySTRef' ra (withD (g yd)) -- 5)
    pure ry

op2 :: Num dc =>
       (a -> b -> (c
                  , dc -> da -> da
                  , dc -> db -> db))
    -> AD s r a da
    -> AD s r b db
    -> AD s r c dc
op2 f ioa iob = do
  ra <- ioa
  rb <- iob
  (D xa _) <- lift $ readSTRef ra
  (D xb _) <- lift $ readSTRef rb
  let (xc, g, h) = f xa xb
  shiftT $ \ k -> lift $ do
    rc <- var xc
    ry <- k rc
    (D _ yd) <- readSTRef rc
    modifySTRef' ra (withD (g yd))
    modifySTRef' rb (withD (h yd))
    pure ry

plus :: (Num a, Num da) => AD s r a da -> AD s r a da -> AD s r a da
plus = op2 (\x y -> (x + y, (+), (+)))

instance (Num a, Num da) => Num (AD s r a da) where
  (+) = plus


-- | 
--
-- >>> rad1 (\x -> x + x) 2
--
-- D 2 0   -- FIXME this should evaluate to (D 4 2)
rad1 :: (Num da) =>
        (forall s . AD s (DVar s a da) a da -> AD s (DVar s a da) a da) -> a -> D a da
rad1 f x = runAD $ do
  let ioa = lift $ var x
  resetT $ do
    let
      iob = f ioa
    zr <- iob
    lift $ modifySTRef' zr (withD (const 1))
    pure zr
  ioa





-- -- univariate RAD
-- rad1 :: (Num da) => (forall s. RAD s a da -> RAD s a da) -> a -> D a da
-- rad1 f x = evalAD $ do
--   let z = pure (D x 0)
--   z' <- resetT (
--     withD (const 1) <$> f z -- reset  { f(z).d = 1.0 }
--     )
--   pure z'


-- -- multivariate RAD

-- rad :: (Num da) => (forall s . AD s [D a da] (D a da) -> RAD s a da) -> [a] -> [D a da] -- ?
-- rad f xs = evalAD $ do
--   let
--     xsl = toList xs
--     n = length xs
--     -- zs :: forall s. AD s (D a da) [D a da] --
--     zs = pure $ zipWith D xsl $ replicate n 0
--   resetT $ f zs






-- --


-- an intermediate abstraction, inspired by 'backprop'. Simplifies the type signatures but I'm not sure it's relevant here.
-- newtype Op s as b = Op {
--   runOpWith :: SDRec s as -> ContT (DVar s b b) (ST s) (b, SDRec s as)
--       }

-- type Op1 s a b = Op s '[a] b
-- type Op2 s a b c = Op s '[a, b] c

{- Op composition ?
(.) :: Op1 s '[b] c -> Op1 s '[a] b -> Op1 s '[a] c
-}




-- -- | from http://conway.rutgers.edu/~ccshan/wiki/blog/posts/Differentiation/
-- instance (Num a) => Num (D a a) where
--   D x x' + D y y' = D (x + y) (x' + y')
--   D x x' * D y y' = D (x * y) (x' * y + x * y')
--   negate (D x x') = D (negate x) (negate x')
--   abs    (D x x') = D (abs x) (signum x * x')
--   signum (D x _)  = D (signum x) 0
--   fromInteger x   = D (fromInteger x) 0

-- instance Fractional a => Fractional (D a a) where
--   recip (D x x') = D (recip x) (-x'/x/x)
--   fromRational x = D (fromRational x) 0


 -- -- | Sum
-- --
-- -- from https://www.cs.purdue.edu/homes/rompf/papers/wang-icfp19.pdf
-- plusD :: Num t =>
--          STRef s (D t) -> STRef s (D t) -> AD s (D t) (STRef s (D t))
-- plusD this that = AD $ do
--   dd1@(D x0 _) <- lift $ readSTRef this
--   dd2@(D x1 _) <- lift $ readSTRef that
--   shiftT $ \k -> lift $ do
--     -- allocate temp variable
--     y <- newSTRef $ D (x0 + x1) 0
--     -- apply continuation
--     (D _ yd) <- k y
--     -- this.d += y.d;
--     modifySTRef' this (withD (+ yd))
--     -- that.d += y.d
--     modifySTRef' that (withD (+ yd))
--     pure $ dd1 + dd2

-- -- | Product
-- timesD :: Num t =>
--           STRef s (D t) -> STRef s (D t) -> AD s (D t) (STRef s (D t))
-- timesD this that = AD $ do
--   dd1@(D x0 _) <- lift $ readSTRef this
--   dd2@(D x1 _) <- lift $ readSTRef that
--   shiftT $ \k -> lift $ do
--     -- allocate temp variable
--     y <- newSTRef $ D (x0 + x1) 0
--     -- apply continuation
--     (D _ yd) <- k y
--     -- this.d += that.x * y.d
--     modifySTRef' this (withD (+ (x1 * yd)))
--     -- that.d +=this.x*y.d
--     modifySTRef' this (withD (+ (x0 * yd)))
--     pure $ dd1 * dd2 

-- read :: STRef s a -> AD s r a
-- read r = AD $ lift $ readSTRef r
-- write :: STRef s a -> a -> AD s r ()
-- write r x = AD $ lift $ writeSTRef r x
-- modify :: STRef s a -> (a -> a) -> AD s r ()
-- modify r f = AD $ lift $ modifySTRef' r f
-- new :: a -> AD s r (STRef s a)
-- new x = AD $ lift $ newSTRef x


-- -- | Dual numbers DD (alternative take, using a type family for the first variation)
-- data DD a = Dd a (Adj a)
-- class Diff a where type Adj a :: *
-- instance Diff Double where type Adj Double = Double
