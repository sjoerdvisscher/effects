{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Control.Effects.Cont where

import Control.Effects

shift :: AutoLift r m n => Effect r m -> ((m a -> m r) -> m r) -> n a
shift p c = operation p $ \k -> c (>>= k)

reset :: Monad m => Handler a a m a
reset = Handler
  { ret = return
  , fin = return
  }