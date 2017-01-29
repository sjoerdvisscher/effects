import Control.Effects
import Control.Effects.Cont
import Control.Effects.Either
import Control.Effects.Error
import Control.Effects.State
import Control.Effects.Writer
import Control.Effects.NonDet

import qualified Data.Set as Set
import Data.Monoid


testIO :: IO ()
testIO = runBase $ do
  base $ putStrLn "What's your name?"
  name <- base getLine
  base $ putStrLn $ "Hello, " ++ name

testRefIO :: IO ()
testRefIO = runBase $ do
  with (ref (5::Int)) $ \x -> do
    val <- get x
    base $ print val

testRef :: (Int, Int)
testRef = run $ do
  with (ref 5) $ \x -> do
    with (ref 10) $ \y -> do
      x =: (+) <$> get x <*> get y
      y =: (+) <$> get x <*> get y
      (,) <$> get x <*> get y


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
    return $ x * x - y * z * x + z * z * z - y * y * x == (0::Int)


testDfs :: [Int] -> [(Int, Int, Int)]
testDfs = run . with (dfs return) . triples

testBfs :: [Int] -> [(Int, Int, Int)]
testBfs = run . with (bfs return) . triples

triples :: (Num a, Eq a, Monoid e, AutoLift e m n) => [a] -> Effect e m -> n (a, a, a)
triples range s = do
  x <- choose s range
  y <- choose s range
  z <- choose s range
  guard s $ x*x + y*y == z*z
  return (x,y,z)


testError :: IO ()
testError = runBase $ do
  with (catchError (\e -> base $ putStrLn ("Error: " ++ e))) $ \c -> do
    base $ putStrLn "before"
    _ <- throwError c "123"
    base $ putStrLn "after"


testEither :: IO ()
testEither = runBase $ do
  with (catchEither (\e -> base $ putStrLn ("Error: " ++ e))) $ \c -> do
    base $ putStrLn "before"
    _ <- throwEither c "123"
    base $ putStrLn "after"


testReset1 :: Int
testReset1 = run $ do
  with reset $ \r -> do
    x <- shift r (\k -> k (k (k (return 7))))
    return $ x * 2 + 1

testReset2 :: IO ()
testReset2 = runBase $ do
  r <- with reset $ \promptA -> do
    base $ putStrLn "Batman"
    with reset $ \promptB -> do
      shift promptB $ \k -> k (k (shift promptA $ \l -> l (l (return ()))))
      base $ putStrLn "Robin"
    base $ putStrLn "Cat woman"
  base $ print r
