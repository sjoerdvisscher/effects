{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.NonDet (choose, nondet, set) where

import Control.Effects
import qualified Data.Set as Set
import Data.Foldable
import Data.Monoid
import Control.Monad

newtype WrapMonad m r = WrapMonad { unwrapMonad :: m r }
instance (Monad m, Monoid r) => Monoid (WrapMonad m r) where
  mempty                              = WrapMonad $ return mempty
  mappend (WrapMonad a) (WrapMonad b) = WrapMonad $ liftM2 mappend a b

choose :: (AutoLift r m n, Monoid r, Foldable f) => Effect r m -> f a -> n a
choose p as = operation p $ \k -> unwrapMonad $ foldMap (WrapMonad . k) as

nondet :: (Monad m, Monoid r) => (a -> r) -> Handler r r m a
nondet f = Handler
  { ret = return . f
  , fin = return
  }

set :: (Monad m, Ord a) => Handler (Set.Set a) (Set.Set a) m a
set = nondet Set.singleton