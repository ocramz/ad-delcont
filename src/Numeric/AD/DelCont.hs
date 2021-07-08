{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Numeric.AD.DelCont where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (Bifunctor(..))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')

-- transformers
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Cont (Cont, shift, reset, evalCont, ContT(..), shiftT, resetT, evalContT, runContT)

import Prelude hiding (read)

-- differentiable variable
newtype DVar s a da = DVar { unDVar :: STRef s (D a da) }

-- | Introduce a fresh DVar
var :: Num da => a -> ST s (DVar s a da)
var x = DVar <$> newSTRef (D x 0)

-- | Dual numbers
data D a da = D a da deriving (Show)
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

-- | from http://conway.rutgers.edu/~ccshan/wiki/blog/posts/Differentiation/
instance (Num a) => Num (D a a) where
  D x x' + D y y' = D (x + y) (x' + y')
  D x x' * D y y' = D (x * y) (x' * y + x * y')
  negate (D x x') = D (negate x) (negate x')
  abs    (D x x') = D (abs x) (signum x * x')
  signum (D x _)  = D (signum x) 0
  fromInteger x   = D (fromInteger x) 0

instance Fractional a => Fractional (D a a) where
  recip (D x x') = D (recip x) (-x'/x/x)
  fromRational x = D (fromRational x) 0

newtype AD s r a = AD { unAD ::  ContT r (ST s) a } deriving (Functor, Applicative, Monad)
evalAD :: (forall s . AD s a a) -> a
evalAD go = runST (evalContT $ unAD go)

runAD :: (forall s . AD s (D a da) (DVar s a da)) -> D a da
runAD go = runST $ (runContT . unAD) go (readSTRef . unDVar)



{-| https://papers.nips.cc/paper/2018/file/34e157766f31db3d2099831d348a7933-Paper.pdf

class NumR(valx: Double,vard: Double) {
  def + (that: NumR) = shift {(k:NumR=>Unit) =>
    val y = new NumR(x + that.x, 0.0);
    k(y);
    this.d += y.d;
    that.d += y.d}
  def * (that: NumR) = shift {(k:NumR=>Unit)=>
    val y = new NumR(x*that.x, 0.0);
    k(y)
    this.d += that.x*y.d;
    that.d +=this.x*y.d
  }}
-}

-- alternative implementation to unOp, passing DVar's
unOp' :: Num da1 =>
         (a1 -> (a2, t -> da2 -> da2))
      -> ContT (D a3 t) (ST s) (DVar s a1 da2)
      -> ContT (D a3 t) (ST s) (DVar s a2 da1)
unOp' f ioa = do
  (DVar ra) <- ioa
  (D xa _) <- lift $ readSTRef ra
  let (xw, g) = f xa
  rc <- shiftT $ \ k -> lift $ do
    y <- var xw
    y'@(D _ yd) <- k y
    modifySTRef' ra (withD (g yd))
    pure y'
  pure rc

unOp :: Num da1 =>
        (a1 -> (a2, t -> da2 -> da2)) -- ^ (forward result, adjoint update)
     -> AD s (D a3 t) (D a1 da2)
     -> AD s (D a3 t) (D a2 da1)
unOp f (AD ioa) = AD $ do
  a@(D xa _) <- ioa
  let (xw, g) = f xa
  ra <- lift $ newSTRef a
  rc <- shiftT $ \ k -> lift $ do
    y <- newSTRef (D xw 0)
    y'@(D _ yd) <- k y
    modifySTRef' ra (withD (g yd))
    pure y'
  lift $ readSTRef rc

-- | Will this work?
binOp :: Num da1 =>
         (a1 -> a2 -> (a3, t -> da2 -> da2)) -- ^ (forward result, adjoint update)
      -> AD s (D a4 t) (D a1 da2)
      -> AD s (D a4 t) (D a2 da2)
      -> AD s (D a4 t) (D a3 da1)
binOp f (AD ioa) (AD iob) = AD $ do
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


-- bla :: (Monad m, Num a) => (a -> m b) -> m b
bla :: ContT r m Integer
bla = ContT $ \k -> do
  let
    a = 1 + 2
  k a

type RAD s a da = AD s (D a da) (DVar s a da)

-- grad :: (forall s . RAD s a da -> RAD s a da) -> a -> (D a da)
-- grad f x = runAD $ AD $ do
--   let z = lift $ var x
--   resetT $ f z

-- grad :: Num a => (a -> a) -> a -> D a
-- grad f x = runAD $ AD $ do
--   z <- lift $ newSTRef $ D x 0
--   -- reset  { f(z).d = 1.0 }
--   resetT $ lift $ modifySTRef' z (withD (const 1) . withX f) -- FIXME update is not pure
--   -- resetT $ \ ioa -> do
--   --   x <- ioa
--   --   f x
--   lift $ readSTRef z



-- --

-- read :: STRef s a -> AD s r a
-- read r = AD $ lift $ readSTRef r
-- write :: STRef s a -> a -> AD s r ()
-- write r x = AD $ lift $ writeSTRef r x
-- modify :: STRef s a -> (a -> a) -> AD s r ()
-- modify r f = AD $ lift $ modifySTRef' r f
-- new :: a -> AD s r (STRef s a)
-- new x = AD $ lift $ newSTRef x
