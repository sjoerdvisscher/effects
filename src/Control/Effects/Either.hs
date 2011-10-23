{-# LANGUAGE MultiParamTypeClasses, GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module Control.Effects.Either where

import Control.Effects
import Data.Void

data EitherEff :: * -> (* -> *) -> * where 
  CatchEither :: (e -> m a) -> EitherEff (Either e a) m
instance Monad m => Effect EitherEff (Either e a) a m a where
  ret (CatchEither _) = return . return
  fin (CatchEither h) = either h return

throwEither :: (AutoLift (ContT (Either e r) m) n, Monad m) => EitherEff (Either e r) m -> e -> n Void
throwEither p e = operation p $ \_ -> return $ Left e