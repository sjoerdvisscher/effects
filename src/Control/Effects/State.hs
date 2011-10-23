{-# LANGUAGE MultiParamTypeClasses, GADTs, FlexibleContexts, FlexibleInstances #-}
module Control.Effects.State where

import Control.Effects

data State e m where Ref :: s -> State (s -> m a) m
instance Monad m => Effect State (s -> m a) a m a where
  ret _ = return . return . return
  fin (Ref s_init) f = f s_init

get :: (c ~ ContT (s -> m r) m, AutoLift c n, Monad m) => State (s -> m r) m -> n s
get p = operation p $ \k -> return $ \s -> do r <- k s; r s

put :: (c ~ ContT (s -> m r) m, AutoLift c n, Monad m) => State (s -> m r) m -> s -> n ()
put p s' = operation p $ \k -> return $ \_ -> do r <- k (); r s'
