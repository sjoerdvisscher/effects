module Main where

import Control.Effects
import Control.Effects.Cont
import Control.Effects.Either
-- import Control.Effects.Error
import Control.Effects.State
import Control.Effects.Writer
import Control.Effects.Set

import qualified Data.Set as Set


testIO :: IO ()
testIO = runIO $ do
  io $ putStrLn "What's your name?"
  name <- io getLine
  io $ putStrLn $ "Hello, " ++ name


testRef :: (Int, Int)
testRef = run $ do
  with (Ref 10) $ \u -> do
    with (Ref 20) $ \v -> do
      w <- get u
      put u (w + 5)
      x <- get v
      put v (x + 1)
      y <- get u
      z <- get v
      return (y, z)


testWriter :: (String, (String, Int))
testWriter = run $ do
  with Writer $ \w1 -> do
    with Writer $ \w2 -> do
      tell w1 "123"
      tell w2 "abc"
      tell w1 "456"
      tell w2 "def"
      return 1


testSet :: Set.Set Int
testSet = run $
  with Set $ \s -> do
    x <- choose s [1, 2]
    y <- choose s [1, 2]
    z <- choose s [1, 2]
    return $ x * x - y * z * x + z * z * z - y * y * x
  

-- testError :: IO ()
-- testError = runIO $ do
--   with (CatchError (\e -> io $ putStrLn ("Error: " ++ e))) $ \c -> do
--     io $ putStrLn "before"
--     throwError c "123"
--     io $ putStrLn "after"
  

testEither :: IO ()
testEither = runIO $ do
  with (CatchEither (\e -> io $ putStrLn ("Error: " ++ e))) $ \c -> do
    io $ putStrLn "before"
    throwEither c "123"
    io $ putStrLn "after"


testReset1 :: Int
testReset1 = run $ do
  with Reset $ \r -> do
    x <- shift r (\k -> k (k (k (return 7))))
    return $ x * 2 + 1

testReset2 :: IO ()
testReset2 = runIO $ do
  r <- with Reset $ \promptA -> do
    io $ putStrLn "Batman"
    with Reset $ \promptB -> do
      shift promptB $ \k -> k (k (shift promptA $ \l -> l (l (return ()))))
      io $ putStrLn "Robin"
    io $ putStrLn "Cat woman"
  io $ print r
