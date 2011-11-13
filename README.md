Control.Effects
===============

Control.Effects is a Haskell library for programming with effects, like in the the [Eff language][Eff] by Andrej Bauer and Matija Pretnar. Effects can be used instead of monad transformers, they are designed to be easier to use and to define.

Installation
------------

    cabal install effects

Using effects
-------------

Here's an example how to use the state effect from `Control.Effects.State`.

    example :: (Int, Int)
    example = run $ do
      with (ref 5) $ \x -> do
        with (ref 10) $ \y -> do
          x =: (+) <$> get x <*> get y
          y =: (+) <$> get x <*> get y
          (,) <$> get x <*> get y

Every instance of an effect is given a name (`x` and `y` in this example), which makes is possible to easily mix several instances of the same effect.

For more examples see [examples.hs](https://github.com/sjoerdvisscher/effects/blob/master/examples.hs).

[Eff]: http://math.andrej.com/category/programming/eff/?category_name=programming/eff