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
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (Foldable(..))
import Data.Functor (void)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')

-- transformers
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Cont (Cont, shift, reset, evalCont, ContT(..), shiftT, resetT, evalContT, runContT)

import Prelude hiding (read)

-- product type (simplified version of vinyl's Rec)
data Rec :: [*] -> * where
  RNil :: Rec '[]
  (:*) :: !a -> !(Rec as) -> Rec (a ': as)

data SDRec s as where
  SDNil :: SDRec s '[]
  (:&) :: DVar s a a -> !(SDRec s as) -> SDRec s (a ': as)


-- newtype Op s as a = Op { runOpWith :: ARec s as -> (a, a -> ARec s as)}

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

-- | Dual numbers (alternative take)
class Diff a where type Adj a :: *
instance Diff Double where type Adj Double = Double
data DD a = Dd a (Adj a)

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

type AD s r a = ContT r (ST s) a -- (Functor, Applicative, Monad)
evalAD :: (forall s . AD s a a) -> a
evalAD go = runST (evalContT go)
-- runAD :: (forall s . AD s (D a da) (DVar s a da)) -> D a da --
-- runAD go = runST $ runContT go (readSTRef . unDVar)



{-| https://papers.nips.cc/paper/2018/file/34e157766f31db3d2099831d348a7933-Paper.pdf
https://www.cs.purdue.edu/homes/rompf/papers/wang-icfp19.pdf

class NumR(valx: Double,vard: Double) {

  def + (that: NumR) = shift {(k:NumR=>Unit) =>
    val y = new NumR(x + that.x, 0.0);
    k(y);
    this.d += y.d;
    that.d += y.d}

  def * (that: NumR) = shift {(k:NumR=>Unit)=>
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
-- in this case we pass DVar's around (?)
op1 :: Num b =>
       (a -> (b
            , b -> a -> a)) -- ^ (result, sensitivity)
    -> Op s '[a] b
op1 f = Op $ \(ra :& SDNil) -> do
  (D xa _) <- lift $ readSTRef ra
  let (xb, g) = f xa
  void $ shiftT $ \ k -> lift $ do
    rb <- var xb
    ry <- k rb -- continuation
    (D _ yd) <- readSTRef ry
    modifySTRef' ra (withD (g yd))
    pure ry
  pure (xb, ra :& SDNil)

op2 :: Num c =>
       (a -> b -> (c
                 , c -> a -> a
                 , c -> b -> b))
    -> Op s '[a, b] c
op2 f = Op $ \(ra :& (rb :& SDNil)) -> do
  (D xa _) <- lift $ readSTRef ra
  (D xb _) <- lift $ readSTRef rb
  let (xc, ga, gb) = f xa xb
  void $ shiftT $ \ k -> lift $ do
    rc <- var xc
    ry <- k rc
    (D _ yd) <- readSTRef ry
    modifySTRef' ra (withD (ga yd))
    modifySTRef' rb (withD (gb yd))
    pure ry
  pure (xc, ra :& (rb :& SDNil))

-- an intermediate abstraction, inspired by 'backprop'. Simplifies the type signatures but I'm not sure it's relevant here.
newtype Op s as b = Op {
  runOpWith :: SDRec s as -> ContT (DVar s b b) (ST s) (b, SDRec s as)
      }

type AD2 s as b = ContT (SDRec s as) (ST s) (b, SDRec s as)

-- liftOp1 :: Op s '[a1] a2
--         -> ContT (DVar s a2 a2) (ST s) (DVar s a1 a1)
--         -> ContT (DVar s a2 a2) (ST s) (SDRec s '[a1])
-- liftOp1 (Op opf) io = do
--   x <- io
--   snd <$> opf (x :& SDNil)

-- liftOp2 :: Op s '[a1, a2] a3
--         -> ContT (DVar s a3 a3) (ST s) (DVar s a1 a1)
--         -> ContT (DVar s a3 a3) (ST s) (DVar s a2 a2)
--         -> ContT (DVar s a3 a3) (ST s) (SDRec s '[a1, a2])
-- liftOp2 (Op opf) io1 io2 = do
--   x1 <- io1
--   x2 <- io2
--   let r = x1 :& (x2 :& SDNil)
--   snd <$> opf r

type Op1 s a b = Op s '[a] b
type Op2 s a b c = Op s '[a, b] c

{- Op composition

(.) :: Op s '[b] c -> Op s '[a] b -> Ops s '[a] c
-}

-- liftOp1 (ra :& SDNil) = undefined


unOp :: Num da1 =>
        (a1 -> (a2, t -> da2 -> da2)) -- ^ (forward result, adjoint update)
     -> AD s (D a3 t) (D a1 da2)
     -> AD s (D a3 t) (D a2 da1)
unOp f ioa = do
  a@(D xa _) <- ioa
  let (xw, g) = f xa
  ra <- lift $ newSTRef a
  rc <- shiftT $ \ k -> lift $ do
    y <- newSTRef (D xw 0)
    y'@(D _ yd) <- k y
    modifySTRef' ra (withD (g yd)) -- FIXME what happens to ra after this line?
    pure y'
  lift $ readSTRef rc

-- | Will this work?
-- --
-- -- adapted from https://www.cs.purdue.edu/homes/rompf/papers/wang-icfp19.pdf
binOp :: Num da1 =>
         (a1 -> a2 -> (a3, t -> da2 -> da2)) -- ^ (forward result, adjoint update)
      -> AD s (D a4 t) (D a1 da2)
      -> AD s (D a4 t) (D a2 da2)
      -> AD s (D a4 t) (D a3 da1)
binOp f ioa iob = do
  a@(D xa _) <- ioa
  b@(D xb _) <- iob
  let (xw, g) = f xa xb
  ra <- lift $ newSTRef a
  rb <- lift $ newSTRef b
  rc <- shiftT $ \ k -> lift $ do
    y <- newSTRef (D xw 0)
    -- apply continuation
    y'@(D _ yd) <- k y
    -- modify operands with continuation result
    modifySTRef' ra (withD (g yd))
    modifySTRef' rb (withD (g yd))
    pure y'
  lift $ readSTRef rc

plus :: (Num a, Num b) =>
        AD s (D a b) (D a b)
     -> AD s (D a b) (D a b)
     -> AD s (D a b) (D a b)
plus = binOp (\a b -> (a + b, (+)))

instance (Num a, Num b) => Num (AD s (D a b) (D a b)) where
  (+) = plus




-- type RAD s a da = AD s (D a da) (DVar s a da)
type RAD s a da = AD s (D a da) (D a da)

rad1 :: (Num da) => (forall s. RAD s a da -> RAD s a da) -> a -> D a da
rad1 f x = evalAD $ do
  let z = pure (D x 0)
  -- z' <- resetT $ f z
  z' <- resetT (
    withD (const 1) <$> f z -- reset  { f(z).d = 1.0 }
    )
  pure z'

-- rad2 f x y = evalAD $ do


-- rad :: (Num da) => (forall s . AD s [D a da] (D a da) -> RAD s a da) -> [a] -> [D a da]
-- rad f xs = evalAD $ do
--   let
--     xsl = toList xs
--     n = length xs
--     -- zs :: forall s. AD s (D a da) [D a da] --
--     zs = pure $ zipWith D xsl $ replicate n 0
--   resetT $ f zs






-- --




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



