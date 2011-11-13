{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module Control.Effects.NonDet (choose, dfs, set, alternatives, accumulate, bfs) where

import Control.Effects
import qualified Data.Set as Set
import Prelude hiding (foldr)
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

dfs :: (Monad m, Monoid r) => (a -> r) -> Handler r r m a
dfs f = Handler
  { ret = return . f
  , fin = return
  }

set :: (Monad m, Ord a) => Handler (Set.Set a) (Set.Set a) m a
set = dfs Set.singleton

alternatives :: (Monad m, Alternative f) => Handler (WrappedAlt f a) (f a) m a
alternatives = accumulate (WrapAlt . pure)

accumulate :: (Monad m, Newtype n o) => (a -> n) -> Handler n o m a
accumulate f = Handler
  { ret = return . f
  , fin = return . unpack
  }

newtype BFS r = BFS { unBFS :: Int -> Maybe r }
instance Monoid r => Monoid (BFS r) where
  mempty                = BFS $ \d -> if d == 0 then Just mempty else Nothing
  BFS f `mappend` BFS g = BFS $ \d -> if d == 0 then f d else f d `mappend` g (d - 1)
instance Monoid r => Newtype (BFS r) r where
  pack r                = BFS $ \d -> if d == 0 then Just r else Nothing
  unpack (BFS f)        = loop 0 where loop d = maybe mempty (`mappend` loop (d + 1)) (f d)

bfs :: (Monad m, Monoid r) => (a -> r) -> Handler (BFS r) r m a
bfs f = accumulate (pack . f)