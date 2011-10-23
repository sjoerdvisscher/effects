{-# LANGUAGE MultiParamTypeClasses, GADTs, KindSignatures, FlexibleContexts, FlexibleInstances #-}
module Control.Effects.Writer where

import Control.Effects
import Data.Monoid

data Writer :: * -> (* -> *) -> * where Writer :: Writer (w, a) m
instance (Monoid w, Monad m) => Effect Writer (w, a) (w, a) m a where
  ret _ a = return (mempty, a)
  fin _ = return

tell :: (c ~ ContT (w, r) m, AutoLift c n, Monad m, Monoid w) => Writer (w, r) m -> w -> n ()
tell p w = operation p $ \k -> do (w', r) <- k (); return (w `mappend` w', r)