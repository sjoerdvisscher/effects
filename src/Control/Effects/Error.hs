{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.Error where

import Control.Effects
import Data.Void

throwError :: (m ~ Program es, Monad m, ess ~ ((e -> m r) -> m r, es), AutoLift ess ds) 
           => Effect ess -> e -> Program ds Void
throwError p e = operation p $ \_ -> return $ \h -> h e

catchError :: (m ~ Program es, Monad m) => (e -> m a) -> Handler ((e -> m a) -> m a, es) a a
catchError h = Handler
  { ret = return . return . return
  , fin = \f -> f h
  }