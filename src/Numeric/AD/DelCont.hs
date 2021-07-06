{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Numeric.AD.DelCont where

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')

-- transformers
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Cont (Cont, shift, reset, evalCont, ContT, shiftT, resetT, evalContT, runContT)

import Prelude hiding (read)

data D a = D a a deriving (Show)

withX :: (a -> a) -> D a -> D a
withX fx (D x d) = D (fx x) d
withD :: (t -> t) -> D t -> D t
withD fd (D x d) = D x (fd d)

instance Eq a => Eq (D a) where
  D x _ == D y _ = x == y

instance Ord a => Ord (D a) where
  compare (D x _) (D y _) = compare x y

-- from http://conway.rutgers.edu/~ccshan/wiki/blog/posts/Differentiation/
instance Num a => Num (D a) where
  D x x' + D y y' = D (x + y) (x' + y')
  D x x' * D y y' = D (x * y) (x' * y + x * y')
  negate (D x x') = D (negate x) (negate x')
  abs    (D x x') = D (abs x) (signum x * x')
  signum (D x _)  = D (signum x) 0
  fromInteger x   = D (fromInteger x) 0

instance Fractional a => Fractional (D a) where
  recip (D x x') = D (recip x) (-x'/x/x)
  fromRational x = D (fromRational x) 0

newtype AD s r a = AD { unAD ::  ContT r (ST s) a } deriving (Functor, Applicative, Monad)
runAD :: (forall s . AD s a a) -> a
runAD go = runST (evalContT $ unAD go)

read :: STRef s a -> AD s r a
read r = AD $ lift $ readSTRef r
write :: STRef s a -> a -> AD s r ()
write r x = AD $ lift $ writeSTRef r x
modify :: STRef s a -> (a -> a) -> AD s r ()
modify r f = AD $ lift $ modifySTRef' r f
new :: a -> AD s r (STRef s a)
new x = AD $ lift $ newSTRef x

newtype DRef s a = DRef (STRef s (D a))

-- from https://www.cs.purdue.edu/homes/rompf/papers/wang-icfp19.pdf
plusD :: Num a =>
         STRef s (D a) -> STRef s (D a) -> AD s (D a) (STRef s (D a))
plusD this that = AD $ do
  dd1@(D x0 _) <- lift $ readSTRef this
  dd2@(D x1 _) <- lift $ readSTRef that
  shiftT $ \k -> lift $ do
    -- allocate temp variable
    y <- newSTRef $ D (x0 + x1) 0
    -- apply continuation
    (D _ y2) <- k y
    -- this.d += y.d;
    modifySTRef' this (withD (+ y2))
    -- that.d += y.d
    modifySTRef' that (withD (+ y2))
    pure $ dd1 + dd2


-- grad :: Num a => (a -> a) -> a -> AD s r (D a)
-- grad :: Num a => (a -> a) -> a -> D a
grad f x = runAD $ AD $ do
  z <- lift $ newSTRef $ D x 0
  -- reset  { f(z).d = 1.0 }
  resetT $ lift $ modifySTRef' z (withD (const 1) . withX f) -- FIXME 
  -- resetT $ lift $ do
  --   z <- lift $ readSTRef zref
  lift $ readSTRef z



-- shiftT :: Monad m => ((a -> m r) -> ContT r m r) -> ContT r m a

-- shiftAD inner = AD $ shiftT $ \k -> do
--   y <- runAD inner
--   y' <- lift $ k y
--   pure y'


-- plusD (D x0 d0) (D x1 d1) = do
--   shift $ \ k ->
--     let y = D (x0 + x1) 0
--     in k y
