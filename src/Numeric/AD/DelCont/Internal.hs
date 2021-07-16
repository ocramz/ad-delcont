{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Numeric.AD.DelCont.Internal where

import Control.Monad ((>=>))
import Control.Monad.ST (ST, runST)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (Foldable(..))
import Data.Functor (void)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')

-- transformers
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Cont (Cont, shift, reset, evalCont, ContT(..), shiftT, resetT, evalContT, runContT)
import Control.Monad.Trans.State (StateT, State, evalState, runState, modify, execStateT, evalStateT, runStateT, put)

import Prelude hiding (read)

-- | Dual numbers
data D a da = D a da deriving (Show, Functor)
instance Eq a => Eq (D a da) where
  D x _ == D y _ = x == y
instance Ord a => Ord (D a db) where
  compare (D x _) (D y _) = compare x y
instance Bifunctor D where
  bimap f g (D a b) = D (f a) (g b)

withX :: (a -> b) -> D a da -> D b da
withX = first
withD :: (da -> db) -> D a da -> D a db
withD = second

-- differentiable variable
type DVar s a da = STRef s (D a da)
-- | Introduce a fresh DVar
var :: Num da => a -> ST s (DVar s a da)
var x = newSTRef (D x 0)


type AD s a da = ContT (DVar s a da) (ST s) (DVar s a da)

runAD :: (forall s . AD s a da) -> D a da
runAD go = runST $ runContT go pure >>= readSTRef

op1 :: Num t =>
       (a1 -> (a2, t -> da -> da))
    -> ContT x (ST s) (DVar s a1 da)
    -> ContT x (ST s) (DVar s a2 t)
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

op2 :: Num t =>
       (a1 -> a2 -> (a3, t -> da1 -> da1, t -> da2 -> da2))
    -> ContT a4 (ST s) (STRef s (D a1 da1))
    -> ContT a4 (ST s) (STRef s (D a2 da2))
    -> ContT a4 (ST s) (DVar s a3 t)
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

plus :: (Num a, Num da) => AD s a da -> AD s a da -> AD s a da
plus = op2 (\x y -> (x + y, (+), (+)))


instance (Num a, Num da) => Num (AD s a da) where
  (+) = plus


-- λ> rad1 (\x -> x + x) 2
-- 2
rad1 :: (Num da) =>
        (forall s . AD s a da -> AD s a da) -> a -> da
rad1 f x = runST $ do
  ioa <- var x
  evalContT $
    resetT $ do
      zr <- f (pure ioa)
      lift $ modifySTRef' zr (withD (const 1))
      pure zr
  (D _ x_bar) <- readSTRef ioa
  pure x_bar

-- λ> rad2 (\x y -> x + y + y) 1 1
-- (1,2)
rad2 :: (Num da) =>
        (forall s . AD s a da -> AD s a da -> AD s a da) -> a -> a -> (da, da)
rad2 f x y = runST $ do
  ioa <- var x
  iob <- var y
  evalContT $
    resetT $ do
      zr <- f (pure ioa) (pure iob)
      lift $ modifySTRef' zr (withD (const 1))
      pure zr
  (D _ x_bar) <- readSTRef ioa
  (D _ y_bar) <- readSTRef iob
  pure (x_bar, y_bar)
