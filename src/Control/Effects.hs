{-# LANGUAGE MultiParamTypeClasses, GADTs, TypeFamilies, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune #-}
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
  , Program
  , Effect
  , AutoLift

) where

data Program es a where
  Pure :: a -> Program () a
  IO :: IO a -> Program (IO ()) a
  Effect :: ((a -> Program es e) -> Program es e) -> Program (e, es) a

runEffect :: Program (e, es) a -> (a -> Program es e) -> Program es e
runEffect (Effect f) = f

lift :: Program es a -> Program (e, es) a
lift m = Effect (bind m)

bind :: Program es a -> (a -> Program es b) -> Program es b
bind (Pure a) f = f a
bind (IO m) f = IO $ m >>= (runIO . f)
bind (Effect m) f = Effect $ \k -> m (\a -> runEffect (f a) k)

instance Monad (Program ()) where
  return = Pure
  (>>=) = bind

instance Monad (Program (IO ())) where
  return = IO . return
  (>>=) = bind

instance Monad (Program (e, es)) where
  return a = Effect $ \k -> k a
  (>>=) = bind

-- $rundoc
-- Here's an example how to use the state effect from 'Control.Effects.State'.
--
-- > example :: Int
-- > example = run $ do
-- >   with (ref 10) $ \u -> do
-- >     val <- get u
-- >     put u (val + 5)
-- >     get u

-- | @with@ takes a handler and creates a new @Proxy@ (effect identifier).
--   The @Proxy@ is passed on to a function which can use it to do operations with it.
with :: Handler (e, es) r a -> (Effect (e, es) -> Program (e, es) a) -> Program es r
with h f = runEffect (f Proxy) (ret h) `bind` fin h

-- | Unwrap the result of the top-level effect.
run :: Program () a -> a
run (Pure a) = a


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
data Handler es r a where 
  Handler :: { ret :: a -> Program es e, fin :: e -> Program es r } -> Handler (e, es) r a

-- | Define an operation, which is autolifted so it can be used inside other effects.
operation :: (AutoLift (e, es) ds) => Effect (e, es) -> ((a -> Program es e) -> Program es e) -> Program ds a
operation p f = operation' p f
  where
    operation' :: forall e es ds a. (AutoLift (e, es) ds) => Effect (e, es) -> ((a -> Program es e) -> Program es e) -> Program ds a
    operation' p f = autolift p (Proxy :: Effect ds) (Effect f)


-- | Variant of 'run' that allows I/O effects. (Just the identity function, but it helps the type checker.)
runIO :: Program (IO ()) a -> IO a
runIO (IO m) = m

-- | Convert an 'IO' action to an I/O effect operation.
io :: AutoLift (IO ()) ds => IO a -> Program ds a
io m = autolift (Proxy :: Effect (IO ())) (Proxy :: Effect ds) (IO m)


data Effect es = Proxy

class AutoLift' m1 m2 n1 n2 where
  autolift' :: Effect n1 -> Effect n2 -> Program m1 a -> Program m2 a

instance (m1 ~ m2) => AutoLift' m1 m2 (IO ()) (IO ()) where
  autolift' Proxy Proxy = id
instance (m1 ~ m2) => AutoLift' m1 m2 () () where
  autolift' Proxy Proxy = id

pre :: Effect (e, es) -> Effect es
pre Proxy = Proxy
instance (AutoLift' m1 m2 (IO ()) n) => AutoLift' m1 (r, m2) (IO ()) (s, n) where
  autolift' p1 p2 = lift . autolift' p1 (pre p2)
instance (AutoLift' m1 m2 () n) => AutoLift' m1 (r, m2) () (s, n) where
  autolift' p1 p2 = lift . autolift' p1 (pre p2)

instance (AutoLift' m1 m2 n1 n2) => AutoLift' m1 m2 (r1, n1) (r2, n2) where
  autolift' p1 p2 = autolift' (pre p1) (pre p2)

class AutoLift m1 m2 where
  autolift :: Effect m1 -> Effect m2 -> Program m1 a -> Program m2 a
instance AutoLift' m1 m2 m1 m2 => AutoLift m1 m2 where
  autolift = autolift'