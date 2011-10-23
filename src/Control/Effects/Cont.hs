{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Control.Effects.Cont where

import Control.Effects

data ContEff e (m :: * -> *) = Reset
instance Monad m => Effect ContEff a a m a where
  ret Reset = return
  fin Reset = return

shift :: (AutoLift (ContT r m) n, Monad m) => ContEff r m -> ((m a -> m r) -> m r) -> n a
shift p c = operation p $ \k -> c (>>= k)