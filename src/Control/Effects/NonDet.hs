{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Control.Effects.NonDet (choose, guard, dfs, set, alternatives, accumulate, bfs) where

import Control.Effects
import qualified Data.Set as Set
import Prelude hiding (foldr)
import Control.Applicative
import Control.Newtype.Generics

instance (Applicative m, Semigroup r) => Semigroup (WrappedMonad m r) where
  WrapMonad a <> WrapMonad b = WrapMonad $ liftA2 (<>) a b
instance (Applicative m, Monoid r) => Monoid (WrappedMonad m r) where
  mempty = WrapMonad $ pure mempty

newtype WrappedAlt f a = WrapAlt (f a)
instance Newtype (WrappedAlt m a) where
  type O (WrappedAlt m a) = m a
  pack = WrapAlt
  unpack (WrapAlt a) = a
instance Alternative f => Semigroup (WrappedAlt f a) where
  WrapAlt a <> WrapAlt b = WrapAlt $ a <|> b
instance Alternative f => Monoid (WrappedAlt f a) where
  mempty = WrapAlt empty

choose :: (AutoLift r m n, Monoid r, Foldable f) => Effect r m -> f a -> n a
choose p as = operation p $ \k -> ala' WrapMonad foldMap k as

guard :: (Monoid r, AutoLift r m n) => Effect r m -> Bool -> n ()
guard _ True = return ()
guard p False = choose p []

dfs :: Monad m => (a -> r) -> Handler r r m a
dfs f = Handler
  { ret = return . f
  , fin = return
  }

set :: Monad m => Handler (Set.Set a) (Set.Set a) m a
set = dfs Set.singleton

alternatives :: (Monad m, Alternative f) => Handler (WrappedAlt f a) (f a) m a
alternatives = accumulate (WrapAlt . pure)

accumulate :: (Monad m, Newtype n) => (a -> n) -> Handler n (O n) m a
accumulate f = Handler
  { ret = return . f
  , fin = return . unpack
  }

newtype BFS r = BFS (Int -> Maybe r)
instance Semigroup r => Semigroup (BFS r) where
  BFS f <> BFS g = BFS $ \d -> if d == 0 then f d else f d <> g (d - 1)
instance Monoid r => Monoid (BFS r) where
  mempty = BFS $ \d -> if d == 0 then Just mempty else Nothing
instance Monoid r => Newtype (BFS r) where
  type O (BFS r) = r
  pack r                = BFS $ \d -> if d == 0 then Just r else Nothing
  unpack (BFS f)        = loop 0 where loop d = maybe mempty (<> loop (d + 1)) (f d)

bfs :: (Monad m, Monoid r) => (a -> r) -> Handler (BFS r) r m a
bfs f = accumulate (pack . f)
