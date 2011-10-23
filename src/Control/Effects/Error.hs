{-# LANGUAGE MultiParamTypeClasses, GADTs, KindSignatures, FlexibleContexts, FlexibleInstances #-}
module Control.Effects.Error where

import Control.Effects
import Data.Void

data ErrorEff :: * -> (* -> *) -> * where 
  CatchError :: (e -> m a) -> ErrorEff ((e -> m a) -> m a) m
instance Monad m => Effect ErrorEff ((e -> m a) -> m a) a m a where
  ret _ = return . return . return
  fin (CatchError h) = \f -> f h
  
throwError :: (AutoLift (ContT ((e -> m a) -> m a) m) n, Monad m) => ErrorEff ((e -> m a) -> m a) m -> e -> n Void
throwError p e = operation p $ \_ -> return $ \h -> h e