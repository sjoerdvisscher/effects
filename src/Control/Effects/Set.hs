{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.Set where

import Control.Effects
import qualified Data.Set as Set

choose :: (c ~ ContT (Set.Set r) m, AutoLift c n, Monad m, Ord r) 
       => Proxy c -> [a] -> n a
choose p as = operation p $ \k -> do
  sets <- mapM k as
  return $ Set.unions sets

set :: (Monad m, Ord a) => Handler (Set.Set a) m a (Set.Set a)
set = Handler
  { ret = return . Set.singleton
  , fin = return
  }
