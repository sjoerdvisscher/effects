{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.Either where

import Control.Effects
import Data.Void

throwEither :: (AutoLift (Either e a, es) ds, Monad (Program es)) 
            => Effect (Either e a, es) -> e -> Program ds Void
throwEither p e = operation p $ \_ -> return $ Left e

catchEither :: Monad (Program es) => (e -> Program es a) -> Handler (Either e a, es) a a
catchEither h = Handler
  { ret = return . return
  , fin = either h return
  }