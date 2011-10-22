{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Control.Effects.State where

import Control.Effects

-- get :: (c ~ ContT (s -> m r) m, AutoLift c n, Monad m) => Proxy c -> n s
get p = operation p $ \k -> return $ \s -> do r <- k s; r s

-- put :: (c ~ ContT (s -> m r) m, AutoLift c n, Monad m) => Proxy c -> s -> n ()
put p s' = operation p $ \k -> return $ \_ -> do r <- k (); r s'

ref :: Monad (Program es) => s -> Handler (s -> Program es a, es) a a
ref s_init = Handler
  { ret = return . return . return
  , fin = \f -> f s_init
  }