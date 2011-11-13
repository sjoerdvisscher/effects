{-# LANGUAGE FlexibleContexts #-}
module Control.Effects.State where

import Control.Effects

type State s m a = s -> m a

get :: AutoLift (State s m a) m n => Effect (State s m a) m -> n s
get p = operation p $ \k -> return $ \s -> do r <- k s; r s

put :: AutoLift (State s m a) m n => Effect (State s m a) m -> s -> n ()
put p s = operation p $ \k -> return $ \_ -> do r <- k (); r s

infixr 3 =:
(=:) :: AutoLift (State s m a) m n => Effect (State s m a) m -> n s -> n ()
p =: m = m >>= put p

modify :: AutoLift (State s m a) m n => Effect (State s m a) m -> (s -> s) -> n ()
modify p f = do
  v <- get p
  put p (f v)

local :: AutoLift (State s m a) m n => Effect (State s m a) m -> (s -> s) -> n b -> n b
local p f m = do
  v <- get p
  put p (f v)
  r <- m
  put p v
  return r

ref :: Monad m => s -> Handler (State s m a) a m a
ref s_init = Handler
  { ret = return . return . return
  , fin = \f -> f s_init
  }