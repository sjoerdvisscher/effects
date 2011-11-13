{-# LANGUAGE FlexibleContexts #-}
module Control.Effects.Either where

import Control.Effects
import Data.Void

throwEither :: AutoLift (Either e r) m n => Effect (Either e r) m -> e -> n Void
throwEither p e = operation p $ \_ -> return $ Left e

catchEither :: Monad m => (e -> m a) -> Handler (Either e a) a m a
catchEither h = Handler
  { ret = return . return
  , fin = either h return
  }