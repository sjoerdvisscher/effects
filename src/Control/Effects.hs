{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Control.Effects (Handler(..), with, operation, run, runIO, io, ioHandler, Cont, ContT, Proxy, AutoLift) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont
import Data.Functor.Identity

data Handler e m a r = Handler
  { ret :: a -> m e
  , fin :: e -> m r
  }

with :: Monad m => Handler e m a r -> (Proxy (ContT e m) -> ContT e m a) -> m r
with h f = runContT (f Proxy) (ret h) >>= fin h

operation :: forall m m' n a r. (m ~ ContT r m', AutoLift m n) => Proxy m -> ((a -> m' r) -> m' r) -> n a
operation p f = autolift p (Proxy :: Proxy n) (ContT f)

run :: Cont a a -> a
run m = runCont m id


ioHandler :: Handler a IO a a
ioHandler = Handler return return

runIO :: ContT () IO () -> IO ()
runIO m = with ioHandler (const m)

io :: AutoLift (ContT () IO) n => IO a -> n a
io m = operation (Proxy :: Proxy (ContT () IO)) (m >>=)


data Proxy (m :: * -> *) = Proxy

class AutoLift' m1 m2 n1 n2 where
  autolift' :: Proxy n1 -> Proxy n2 -> m1 a -> m2 a

instance (m1 ~ m2) => AutoLift' m1 m2 IO IO where
  autolift' Proxy Proxy = id
instance (m1 ~ m2) => AutoLift' m1 m2 Identity Identity where
  autolift' Proxy Proxy = id

pre :: Proxy (ContT r m) -> Proxy m
pre Proxy = Proxy
instance (AutoLift' m1 m2 IO n, Monad m2) => AutoLift' m1 (ContT r m2) IO (ContT s n) where
  autolift' p1 p2 = lift . autolift' p1 (pre p2)
instance (AutoLift' m1 m2 Identity n, Monad m2) => AutoLift' m1 (ContT r m2) Identity (ContT s n) where
  autolift' p1 p2 = lift . autolift' p1 (pre p2)

instance (AutoLift' m1 m2 n1 n2) => AutoLift' m1 m2 (ContT r1 n1) (ContT r2 n2) where
  autolift' p1 p2 = autolift' (pre p1) (pre p2)

class AutoLift m1 m2 where
  autolift :: Proxy m1 -> Proxy m2 -> m1 a -> m2 a
instance AutoLift' m1 m2 m1 m2 => AutoLift m1 m2 where
  autolift = autolift'