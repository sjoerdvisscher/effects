{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.Set where

import Control.Effects
import qualified Data.Set as Set

choose :: (AutoLift (Set.Set r) m n, Ord r) => Effect (Set.Set r) m -> [a] -> n a
choose p as = operation p $ \k -> do
  sets <- mapM k as
  return $ Set.unions sets

set :: (Monad m, Ord a) => Handler (Set.Set a) (Set.Set a) m a
set = Handler
  { ret = return . Set.singleton
  , fin = return
  }
