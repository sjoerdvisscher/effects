{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.Either where

import Control.Effects
import Data.Void

throwEither :: (c ~ ContT (m (Either e r)) m, AutoLift c n, Monad m) => Proxy c -> e -> n Void
throwEither p e = operation p $ \_ -> return . return $ Left e

catchEither :: Monad m => (e -> m a) -> Handler (m (Either e a)) m a a
catchEither h = Handler
  { ret = return . return . return
  , fin = \m -> m >>= either h return
  }