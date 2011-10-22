{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Control.Effects.Cont where

import Control.Effects

shift :: (AutoLift (e, es) ds, Monad (Program es)) 
      => Effect (e, es) -> ((Program es a -> Program es e) -> Program es e) -> Program ds a
shift p c = operation p $ \k -> c (>>= k)

reset :: Monad (Program es) => Handler (a, es) a a
reset = Handler
  { ret = return
  , fin = return
  }