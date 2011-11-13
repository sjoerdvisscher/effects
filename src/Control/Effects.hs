{-# LANGUAGE MultiParamTypeClasses, KindSignatures, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
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
  , Base(..)
  , base
  -- * Effects machinery
  , Layer
  , Pure
  , Effect
  , AutoLift
  , AutoLiftBase

) where

import Control.Applicative

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
with h f = runLayer (f Effect) (ret h) >>= fin h

-- | Unwrap the result of the top-level effect.
run :: Base Pure a -> a
run = runPure . runBase


-- $defdoc
-- Here's and example how to define the state effect from 'Control.Effects.Writer'.
--
-- > writer :: (Monad m, Monoid w) => Handler (w, a) (w, a) m a
-- > writer = Handler
-- >   { ret = \a -> return (mempty, a)
-- >   , fin = return
-- >   }
-- > 
-- > tell :: (AutoLift (w, r) m n, Monoid w) => Effect (w, r) m -> w -> n ()
-- > tell p v = operation p $ \k -> do
-- >   (w, r) <- k ()
-- >   return (mappend v w, r)

-- | A @Handler e r m a@ is a handler of effects with type @e@. 
--   The @ret@ field provides a function to lift pure values into the effect.
--   The @fin@ field provides a function to extract a final value of type @r@ from the effect.
--   The parameter @m@ should narmally be left polymorphic, it's the monad that handles the other effects.
data Handler e r m a = Handler
  { ret :: a -> m e
  , fin :: e -> m r
  }

-- | @Layer e m@ is a monad that adds an effect @e@ to the underlying monad @m@.
--   (It is the continuation monad transformer with a friendlier name.)
newtype Layer e m a = Layer { runLayer :: (a -> m e) -> m e }

instance Functor (Layer r m) where
  fmap f m = Layer $ \k -> runLayer m (k . f)

instance Applicative (Layer r m) where
  pure a   = Layer $ \k -> k a
  m <*> v  = Layer $ \k -> runLayer m (\f -> runLayer v (k . f))

instance Monad (Layer e m) where
  return a = Layer $ \k -> k a
  m >>= f  = Layer $ \k -> runLayer m (\a -> runLayer (f a) k)


newtype Pure a = Pure { runPure :: a }

instance Functor Pure where
  fmap f (Pure a) = Pure (f a)

instance Applicative Pure where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)  
  
instance Monad Pure where
  return = Pure
  Pure a >>= f = f a


newtype Base m a = Base { runBase :: m a }

instance Functor m => Functor (Base m) where
  fmap f (Base m) = Base (fmap f m)

instance Applicative m => Applicative (Base m) where
  pure = Base . pure
  Base m <*> Base v = Base (m <*> v)
  
instance Monad m => Monad (Base m) where
  return = Base . return
  Base m >>= f = Base $ m >>= runBase . f

-- | @Effect e m@ is a proxy for the type checker to be able to work with multiple effects at the same time.
data Effect e (m :: * -> *) = Effect


class (Monad m, Monad n) => AutoLift e m n where
  operation :: Effect e m -> ((a -> m e) -> m e) -> n a

instance (Monad m, Monad n, AutoLiftInternal (Layer e m) (Base    n) (Layer e m) (Base    n)) => AutoLift e m (Base    n) where
  operation _ f = autolift (Proxy :: Proxy (Layer e m)) (Proxy :: Proxy (Base    n)) (Layer f)
instance (Monad m, Monad n, AutoLiftInternal (Layer e m) (Layer d n) (Layer e m) (Layer d n)) => AutoLift e m (Layer d n) where
  operation _ f = autolift (Proxy :: Proxy (Layer e m)) (Proxy :: Proxy (Layer d n)) (Layer f)


class (Monad m, Monad n) => AutoLiftBase m n where
  base :: m a -> n a

instance (Monad m, Monad n, AutoLiftInternal (Base    m) (Base    n) (Base    m) (Base    n)) => AutoLiftBase m (Base    n) where
  base m        = autolift (Proxy :: Proxy (Base    m)) (Proxy :: Proxy (Base    n)) (Base m)
instance (Monad m, Monad n, AutoLiftInternal (Base    m) (Layer e n) (Base    m) (Layer e n)) => AutoLiftBase m (Layer e n) where
  base m        = autolift (Proxy :: Proxy (Base    m)) (Proxy :: Proxy (Layer e n)) (Base m)


data Proxy (m :: * -> *) = Proxy

class (Monad m1, Monad m2) =>  AutoLiftInternal m1 m2 n1 n2 where
  autolift :: Proxy n1 -> Proxy n2 -> m1 a -> m2 a

pre :: Proxy (Layer r m) -> Proxy m
pre Proxy = Proxy

instance (Monad m)                             => AutoLiftInternal m           m   (Base    n)  (Base    n)  where
  autolift Proxy Proxy = id
instance (AutoLiftInternal m1 m2 (Base n1) n2) => AutoLiftInternal m1 (Layer r m2) (Base    n1) (Layer s n2) where
  autolift p1 p2 = Layer . (>>=) . autolift p1 (pre p2)
instance (AutoLiftInternal m1 m2       n1  n2) => AutoLiftInternal m1          m2  (Layer r n1) (Layer s n2) where
  autolift p1 p2 = autolift (pre p1) (pre p2)
