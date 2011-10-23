{-# LANGUAGE MultiParamTypeClasses, GADTs, KindSignatures, FlexibleContexts, FlexibleInstances #-}
module Control.Effects.Set where

import Control.Effects
import qualified Data.Set as Set

data Set :: * -> (* -> *) -> * where Set :: Set (Set.Set a) m
instance Monad m => Effect Set (Set.Set a) (Set.Set a) m a where
  ret _ = return . Set.singleton
  fin _ = return

choose :: (c ~ ContT (Set.Set r) m, AutoLift (ContT (Set.Set r) m) n, Monad m, Ord r) 
       => Set (Set.Set r) m -> [a] -> n a
choose p as = operation p $ \k -> do
  sets <- mapM k as
  return $ Set.unions sets