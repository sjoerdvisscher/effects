{-# LANGUAGE FlexibleContexts #-}
module Control.Effects.Writer where

import Control.Effects
import Data.Monoid

tell :: (AutoLift (w, r) m n, Monoid w) => Effect (w, r) m -> w -> n ()
tell p v = operation p $ \k -> do
  ~(w, r) <- k ()
  return (mappend v w, r)

writer :: (Monad m, Monoid w) => Handler (w, a) (w, a) m a
writer = Handler
  { ret = \a -> return (mempty, a)
  , fin = return
  }