{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.State where

import Control.Effects

get :: AutoLift (s -> m r) m n => Effect (s -> m r) m -> n s
get p = operation p $ \k -> return $ \s -> do r <- k s; r s

put :: AutoLift (s -> m r) m n => Effect (s -> m r) m -> s -> n ()
put p s' = operation p $ \k -> return $ \_ -> do r <- k (); r s'

ref :: Monad m => s -> Handler (s -> m a) a m a
ref s_init = Handler
  { ret = return . return . return
  , fin = \f -> f s_init
  }