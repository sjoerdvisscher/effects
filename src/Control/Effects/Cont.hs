{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.Cont where

import Control.Effects

shift :: (c ~ ContT r m, AutoLift c n, Monad m) => Proxy c -> ((m a -> m r) -> m r) -> n a
shift p c = operation p $ \k -> c (>>= k)

reset :: Monad m => Handler a m a a
reset = Handler
  { ret = return
  , fin = return
  }