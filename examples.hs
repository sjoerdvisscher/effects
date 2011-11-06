module Main where

import Control.Effects
import Control.Effects.Cont
import Control.Effects.Either
import Control.Effects.Error
import Control.Effects.State
import Control.Effects.Writer
import Control.Effects.NonDet

import qualified Data.Set as Set
import Control.Newtype
import Data.Monoid


testIO :: IO ()
testIO = runIO $ do
  io $ putStrLn "What's your name?"
  name <- io getLine
  io $ putStrLn $ "Hello, " ++ name


testRef :: (Int, Int)
testRef = run $ do
  with (ref 10) $ \u -> do
    with (ref 20) $ \v -> do
      w <- get u
      put u (w + 5)
      x <- get v
      put v (x + 1)
      y <- get u
      z <- get v
      return (y, z)


testWriter :: (String, (String, Int))
testWriter = run $ do
  with writer $ \w1 -> do
    with writer $ \w2 -> do
      tell w1 "123"
      tell w2 "abc"
      tell w1 "456"
      tell w2 "def"
      return 1


testSet :: Set.Set Int
testSet = run $
  with set $ \s -> do
    x <- choose s [1, 2]
    y <- choose s [1, 2]
    z <- choose s [1, 2]
    return $ x * x - y * z * x + z * z * z - y * y * x

testAccumulate :: Bool
testAccumulate = run $
  with (accumulate Any) $ \s -> do
    x <- choose s [1, 2]
    y <- choose s [1, 2]
    z <- choose s [1, 2]
    return $ x * x - y * z * x + z * z * z - y * y * x == 0


testDfs :: [Int] -> [(Int, Int, Int)]
testDfs = run . with (dfs return) . triples

testBfs :: [Int] -> [(Int, Int, Int)]
testBfs = run . with (bfs return) . triples

triples range s = do
  x <- choose s range
  y <- choose s range
  z <- choose s range
  if x*x + y*y == z*z then return (x,y,z) else choose s []


testError :: IO ()
testError = runIO $ do
  with (catchError (\e -> io $ putStrLn ("Error: " ++ e))) $ \c -> do
    io $ putStrLn "before"
    throwError c "123"
    io $ putStrLn "after"
  

testEither :: IO ()
testEither = runIO $ do
  with (catchEither (\e -> io $ putStrLn ("Error: " ++ e))) $ \c -> do
    io $ putStrLn "before"
    throwEither c "123"
    io $ putStrLn "after"


testReset1 :: Int
testReset1 = run $ do
  with reset $ \r -> do
    x <- shift r (\k -> k (k (k (return 7))))
    return $ x * 2 + 1

testReset2 :: IO ()
testReset2 = runIO $ do
  r <- with reset $ \promptA -> do
    io $ putStrLn "Batman"
    with reset $ \promptB -> do
      shift promptB $ \k -> k (k (shift promptA $ \l -> l (l (return ()))))
      io $ putStrLn "Robin"
    io $ putStrLn "Cat woman"
  io $ print r
