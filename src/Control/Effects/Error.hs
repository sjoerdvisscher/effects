{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.Error where

import Control.Effects
import Data.Void

throwError :: (c ~ ContT ((e -> m r) -> m r) m, AutoLift c n, Monad m) => Proxy c -> e -> n Void
throwError p e = operation p $ \_ -> return $ \h -> h e

catchError :: Monad m => (e -> m a) -> Handler ((e -> m a) -> m a) m a a
catchError h = Handler
  { ret = return . return . return
  , fin = \f -> f h
  }