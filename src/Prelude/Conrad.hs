{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Prelude.Conrad where

import Data.Function (on)
import Data.IORef
import qualified Data.Map as Map
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

newtype Tape = Tape {nodes :: IORef [Node]}

data Var = Var {tape :: Tape, index :: Int, value :: Double}

add :: Var -> Var -> IO Var
add (Var xt xi xv) (Var yt yi yv) = do
  index <- push2 xt xi 1.0 yi 1.0
  return (Var xt index (xv + yv))

mul :: Var -> Var -> IO Var
mul (Var xt xi xv) (Var yt yi yv) = do
  index <- push2 xt xi yv yi xv
  return (Var xt index (xv * yv))

sine :: Var -> IO Var
sine (Var t i v) = do
  index <- push1 t i (cos v)
  return (Var t index (sin v))

newtype Grad = Grad {derivs :: [Double]}

newTape :: IO Tape
newTape = Tape <$> newIORef []

newVar :: Tape -> Double -> IO Var
newVar tape value = do
  index <- push0 tape
  return Var {tape = tape, value = value, index = index}

push0 :: Tape -> IO Int
push0 tape = do
  nodes' <- readIORef (nodes tape)
  let len = length nodes'
      node = Node {weights = [0.0, 0.0], deps = [len, len]}
  writeIORef (nodes tape) (nodes' ++ [node])
  return len

push1 :: Tape -> Int -> Double -> IO Int
push1 tape dep0 weight0 = do
  nodes' <- readIORef (nodes tape)
  let len = length nodes'
      node = Node {weights = [weight0, 0.0], deps = [dep0, len]}
  writeIORef (nodes tape) (nodes' ++ [node])
  return len

push2 :: Tape -> Int -> Double -> Int -> Double -> IO Int
push2 tape dep0 weight0 dep1 weight1 = do
  nodes' <- readIORef (nodes tape)
  let len = length nodes'
      node = Node {weights = [weight0, weight1], deps = [dep0, dep1]}
  writeIORef (nodes tape) (nodes' ++ [node])
  return len

gradV :: Var -> IO Grad
gradV v = do
  nodes' <- readIORef (nodes $ tape v)
  let len = length nodes'
      derivs = replicate (len - 1) 0.0 ++ [1.0]
      derivs' = reverse $ foldl backwardPass derivs [len - 1, len - 2 .. 0]
      backwardPass derivs' i =
        let node = nodes' !! i
            [dep0, dep1] = deps node
            [weight0, weight1] = weights node
            deriv = derivs' !! i
            updated = [deriv + w * derivs' !! dep | (dep, w) <- [(dep0, weight0), (dep1, weight1)]]
         in take i derivs' ++ updated ++ drop (i + 1) derivs'
  return Grad {derivs = derivs}

grad :: ([Var] -> Var) -> ([Double] -> IO [Double])
grad f inputs = do
  tape <- newTape
  vars <- mapM (newVar tape) inputs
  let output = f vars
  grad <- gradV output
  return (derivs grad)
