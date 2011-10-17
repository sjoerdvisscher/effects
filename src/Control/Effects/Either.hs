{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.Either where

import Control.Effects
import Data.Void

throwEither :: (c ~ ContT (Either e r) m, AutoLift c n, Monad m) => Proxy c -> e -> n Void
throwEither p e = operation p $ \_ -> return $ Left e

catchEither :: Monad m => (e -> m a) -> Handler (Either e a) a m a
catchEither h = Handler
  { ret = return . return
  , fin = either h return
  }