{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.Error where

import Control.Effects
import Data.Void

throwError :: AutoLift ((e -> m r) -> m r) m n => Effect ((e -> m r) -> m r) m -> e -> n Void
throwError p e = operation p $ \_ -> return $ \h -> h e

catchError :: Monad m => (e -> m a) -> Handler ((e -> m a) -> m a) a m a
catchError h = Handler
  { ret = return . return . return
  , fin = \f -> f h
  }