{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Control.Effects.NonDet (choose, nondet, set, alternatives, accumulate) where

import Control.Effects
import qualified Data.Set as Set
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Newtype

instance (Monad m, Monoid r) => Monoid (WrappedMonad m r) where
  mempty                              = WrapMonad $ return mempty
  mappend (WrapMonad a) (WrapMonad b) = WrapMonad $ liftM2 mappend a b

newtype WrappedAlt f a = WrapAlt (f a)
instance Newtype (WrappedAlt m a) (m a) where
  pack = WrapAlt
  unpack (WrapAlt a) = a
instance Alternative f => Monoid (WrappedAlt f a) where
  mempty                          = WrapAlt empty
  mappend (WrapAlt a) (WrapAlt b) = WrapAlt $ a <|> b

choose :: (AutoLift r m n, Monoid r, Foldable f) => Effect r m -> f a -> n a
choose p as = operation p $ \k -> ala' WrapMonad foldMap k as

nondet :: (Monad m, Monoid r) => (a -> r) -> Handler r r m a
nondet f = Handler
  { ret = return . f
  , fin = return
  }

set :: (Monad m, Ord a) => Handler (Set.Set a) (Set.Set a) m a
set = nondet Set.singleton

alternatives :: (Monad m, Alternative f) => Handler (WrappedAlt f a) (f a) m a
alternatives = accumulate (WrapAlt . pure)

accumulate :: (Monad m, Newtype n o) => (a -> n) -> Handler n o m a
accumulate f = Handler
  { ret = return . f
  , fin = return . unpack
  }
