{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Prelude.Conrad where

import Control.Monad (forM_, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Function (on)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Debug.Trace (trace, traceIO)
import GHC.TypeLits

mapIdx :: (a -> Int -> b) -> [a] -> [b]
mapIdx f l = zipWith f l [0 ..]

data Scalar a where
  Scalar :: a -> Scalar a

data Vector (n :: Nat) a where
  Vector :: [a] -> Vector n a

data Matrix (m :: Nat) (n :: Nat) a where
  Matrix :: [[a]] -> Matrix m n a

data Tensor (s :: [Nat]) a where
  Tensor :: [a] -> Tensor s a

data Dual a = Dual {primal :: a, tangent :: a}

instance Num a => Num (Dual a) where
  (Dual x x') + (Dual y y') = Dual (x + y) (x' + y')
  (Dual x x') - (Dual y y') = Dual (x - y) (x' - y')
  (Dual x x') * (Dual y y') = Dual (x * y) (x * y' + x' * y)
  abs (Dual x x') = Dual (abs x) (signum x * x')
  signum (Dual x x') = Dual (signum x) 0
  fromInteger x = Dual (fromInteger x) 0

instance Fractional a => Fractional (Dual a) where
  (Dual x x') / (Dual y y') = Dual (x / y) ((x' / y) - (x * y') / (y * y))
  fromRational x = Dual (fromRational x) 0

instance Floating a => Floating (Dual a) where
  pi = Dual pi 0
  exp (Dual x x') = Dual (exp x) (x' * exp x)
  log (Dual x x') = Dual (log x) (x' / x)
  sin (Dual x x') = Dual (sin x) (x' * cos x)
  cos (Dual x x') = Dual (cos x) (-x' * sin x)
  asin (Dual x x') = Dual (asin x) (x' / sqrt (1 - x * x))
  acos (Dual x x') = Dual (acos x) (-x' / sqrt (1 - x * x))
  atan (Dual x x') = Dual (atan x) (x' / (1 + x * x))
  sinh (Dual x x') = Dual (sinh x) (x' * cosh x)
  cosh (Dual x x') = Dual (cosh x) (x' * sinh x)
  tanh (Dual x x') = Dual (tanh x) (x' / (cosh x * cosh x))
  asinh (Dual x x') = Dual (asinh x) (x' / sqrt (1 + x * x))
  acosh (Dual x x') = Dual (acosh x) (x' / sqrt (x * x - 1))
  atanh (Dual x x') = Dual (atanh x) (x' / (1 - x * x))

diffF :: Floating a => (Dual a -> Dual b) -> a -> b
diffF f = tangent . f . (`Dual` 1.0)

gradF :: Floating a => ([Dual a] -> Dual b) -> [a] -> [b]
gradF f xs = map partial idxs
  where
    idxs = [0 .. length xs - 1]
    partial = tangent . f . oneHotDuals
    oneHotDuals i = mapIdx (\x j -> Dual x (if i == j then 1.0 else 0.0)) xs

jacobianF :: Floating a => ([Dual a] -> [Dual b]) -> [a] -> [[b]]
jacobianF f xs = map partial idxs
  where
    idxs = [0 .. length xs - 1]
    partial = map tangent . f . oneHotDuals
    oneHotDuals i = mapIdx (\x j -> Dual x (if i == j then 1.0 else 0)) xs

fn1 :: Floating a => a -> a
fn1 x = (1.0 - y) / (1.0 + y)
  where
    y = x ^ (-2)

fn1' :: Floating a => a -> a
fn1' = diffF fn1

fn2 :: Floating a => [a] -> a
fn2 [x, y] = (sin (x / y) + x / y - exp y) * (x / y - exp y)

fn2' :: Floating a => [a] -> [a]
fn2' = gradF fn2

fn3 :: Floating a => [a] -> [a]
fn3 [x, y] = [x * x * y, x + y]

fn3' :: Floating a => [a] -> [[a]]
fn3' = jacobianF fn3

data Node = Node {weights :: [Double], deps :: [Int]}

newtype Tape s = Tape {nodesRef :: STRef s (VM.MVector s Node)}

data Var s = Var {tape :: Tape s, index :: Int, value :: Double}

insertMVec v i x = do
  let l = VM.length v
  if i >= l
    then do
      -- let growBy = max (l * 2) (i + 1) - l
      let growBy = 1
      v' <- VM.grow v growBy
      VM.write v' i x
      return v'
    else do
      VM.write v i x
      return v

push2 :: Tape s -> Int -> Double -> Int -> Double -> ST s Int
push2 t xi xw yi yw = do
  ns <- readSTRef (nodesRef t)
  let zi = VM.length ns
      z = Node {weights = [xw, yw], deps = [xi, yi]}
  ns' <- insertMVec ns zi z
  writeSTRef (nodesRef t) ns'
  return zi

push1 :: Tape s -> Int -> Double -> ST s Int
push1 t d w = do
  ns <- readSTRef (nodesRef t)
  let oi = VM.length ns
      o = Node {weights = [w, 0.0], deps = [d, oi]}
  ns' <- insertMVec ns oi o
  writeSTRef (nodesRef t) ns'
  return oi

push0 :: Tape s -> ST s Int
push0 t = do
  ns <- readSTRef (nodesRef t)
  let oi = VM.length ns
      o = Node {weights = [0.0, 0.0], deps = [oi, oi]}
  ns' <- insertMVec ns oi o
  writeSTRef (nodesRef t) ns'
  return oi

add :: Var s -> Var s -> ST s (Var s)
add (Var xt xi xv) (Var yt yi yv) = do
  zi <- push2 xt xi 1.0 yi 1.0
  return (Var xt zi (xv + yv))

mul :: Var s -> Var s -> ST s (Var s)
mul (Var xt xi xv) (Var yt yi yv) = do
  zi <- push2 xt xi yv yi xv
  return (Var xt zi (xv * yv))

sinV :: Var s -> ST s (Var s)
sinV (Var t i v) = do
  oi <- push1 t i (cos v)
  return (Var t oi (sin v))

newtype Grad = Grad {derivs :: [Double]}

wrt :: Grad -> Var s -> Double
wrt g v = derivs g !! index v

newTape :: ST s (Tape s)
newTape = Tape <$> (newSTRef =<< VM.new 0)

newVar :: Tape s -> Double -> ST s (Var s)
newVar t v = do
  i <- push0 t
  return (Var t i v)

backward :: Var s -> ST s Grad
backward (Var t i _) = do
  ns <- readSTRef $ nodesRef t
  let len = VM.length ns
  derivs <- VM.replicate len 0.0
  VM.write derivs i 1.0
  forM_ [len - 1, len - 2 .. 0] $ \j -> do
    n <- VM.read ns j
    deriv <- VM.read derivs j
    forM_ [0 .. 1] $ \k -> do
      VM.modify derivs (+ (weights n !! k)) (deps n !! k)
  Grad <$> (V.toList <$> V.freeze derivs)

fVar :: Var s -> ST s (Var s)
fVar x = join (add <$> mul x x <*> sinV x)

fVar' = diffR fVar

diffR :: (Var s -> ST s (Var s)) -> (Var s -> ST s Double)
diffR f x = do
  z <- f x
  grads <- backward z
  return (wrt grads x)

example :: Double -> Double -> IO Grad
example x y = stToIO $ do
  tape <- newTape
  xv <- newVar tape x
  yv <- newVar tape y
  z <- join (add <$> mul xv yv <*> sinV xv)
  backward z

printGrads :: Grad -> IO ()
printGrads (Grad grads) = putStrLn $ "Gradients: [" ++ intercalate ", " (map show grads) ++ "]"

tr :: (Applicative m) => String -> m ()
tr msg = trace msg $ pure ()

trST :: String -> ST s ()
trST msg = unsafeIOToST $ traceIO msg

trIO :: MonadIO m => String -> m ()
trIO msg = liftIO $ traceIO msg

main :: IO ()
main = do
  let x = 0.5
      y = 4.2
  grads <- example x y
  let dzdx = head (derivs grads)
      dzdy = last (derivs grads)
  putStrLn $ "dz = " ++ show (derivs grads)
  putStrLn $ "dz/dx = " ++ show dzdx
  putStrLn $ "dz/dy = " ++ show dzdy
  printGrads grads
