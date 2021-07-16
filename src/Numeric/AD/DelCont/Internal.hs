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
