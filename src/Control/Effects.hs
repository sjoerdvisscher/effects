{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Control.Effects (

  -- * Running effects
  -- $rundoc
    with
  , run
  -- * Defining effects  
  -- $defdoc
  , Handler(..)
  , operation
  -- * I/O
  , runIO
  , io
  -- * Effects machinery
  , Layer
  , Effect
  , AutoLift

) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont
import Data.Functor.Identity

-- $rundoc
-- Here's an example how to use the state effect from 'Control.Effects.State'.
--
-- > example :: Int
-- > example = run $ do
-- >   with (ref 10) $ \u -> do
-- >     val <- get u
-- >     put u (val + 5)
-- >     get u

-- | @with@ takes a handler and creates a new effect instance.
--   The @Effect@ is passed on to a function which can use it to do operations with it.
with :: Monad m => Handler e r m a -> (Effect e m -> Layer e m a) -> m r
with h f = runContT (f Effect) (ret h) >>= fin h

-- | Unwrap the result of the top-level effect.
run :: Identity a -> a
run = runIdentity


-- $defdoc
-- Here's and example how to define the state effect from 'Control.Effects.State'.
--
-- > ref :: Monad m => s -> Handler (s -> m a) a m a
-- > ref s_init = Handler
-- >   { ret = return . return . return
-- >   , fin = \f -> f s_init
-- >   }
-- >
-- > get p   = operation p $ \k -> return $ \s -> do r <- k s; r s
-- > put p s = operation p $ \k -> return $ \_ -> do r <- k (); r s
 
-- | A @Handler e r m a@ is a handler of effects with type @e@. 
--   The @ret@ field provides a function to lift pure values into the effect.
--   The @fin@ field provides a function to extract a final value of type @r@ from the effect.
--   The parameter @m@ should narmally be left polymorphic, it's the monad that handles the other effects.
data Handler e r m a = Handler
  { ret :: a -> m e
  , fin :: e -> m r
  }

-- | Define an operation, which is autolifted so it can be used inside other effects.
operation :: AutoLift e m n => Effect e m -> ((a -> m e) -> m e) -> n a
operation p f = operation' p f 
  where
    -- Workaround to hide the forall from Haddock
    operation' :: forall m n a e. AutoLift e m n => Effect e m -> ((a -> m e) -> m e) -> n a
    operation' _ f = autolift (Proxy :: Proxy (Layer e m)) (Proxy :: Proxy n) (ContT f)


-- | Variant of 'run' that allows I/O effects. (Just the identity function, but it helps the type checker.)
runIO :: IO () -> IO ()
runIO = id

-- | Convert an 'IO' action to an I/O effect operation.
io :: forall n a. AutoLiftInternal IO n IO n => IO a -> n a
io m = autolift' (Proxy :: Proxy IO) (Proxy :: Proxy n) m

-- | @Layer e m@ is a monad that adds an effect @e@ to the underlying monad @m@.
--   It is a type synonym for the continuation monad transformer.
type Layer e m = ContT e m

-- | @Effect e m@ is a proxy for the type checker to be able to work with multiple effects at the same time.
data Effect e (m :: * -> *) = Effect

data Proxy (m :: * -> *) = Proxy

class AutoLiftInternal m1 m2 n1 n2 where
  autolift' :: Proxy n1 -> Proxy n2 -> m1 a -> m2 a

instance (m1 ~ m2) => AutoLiftInternal m1 m2 IO IO where
  autolift' Proxy Proxy = id
instance (m1 ~ m2) => AutoLiftInternal m1 m2 Identity Identity where
  autolift' Proxy Proxy = id

pre :: Proxy (ContT r m) -> Proxy m
pre Proxy = Proxy
instance (AutoLiftInternal m1 m2 IO n, Monad m2) => AutoLiftInternal m1 (ContT r m2) IO (ContT s n) where
  autolift' p1 p2 = lift . autolift' p1 (pre p2)
instance (AutoLiftInternal m1 m2 Identity n, Monad m2) => AutoLiftInternal m1 (ContT r m2) Identity (ContT s n) where
  autolift' p1 p2 = lift . autolift' p1 (pre p2)

instance (AutoLiftInternal m1 m2 n1 n2) => AutoLiftInternal m1 m2 (ContT r1 n1) (ContT r2 n2) where
  autolift' p1 p2 = autolift' (pre p1) (pre p2)

-- | @AutoLift e m n@ lifts an effect @e@ layered on top of monad @m@ to the monad @n@.
class Monad m => AutoLift e m n where
  autolift :: Proxy (Layer e m) -> Proxy n -> Layer e m a -> n a
instance (Monad m, AutoLiftInternal (Layer e m) n (Layer e m) n) => AutoLift e m n where
  autolift = autolift'