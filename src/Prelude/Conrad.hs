{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Prelude.Conrad where

import Data.Function (on)
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

data Expr = Var Int | Const Double | Add Expr Expr | Mul Expr Expr | Sin Expr

data Tape = Tape {output :: Double, varTapes :: Map.Map Int Tape} deriving (Show)

eval :: Map.Map Int Double -> Expr -> Double
eval env (Var x) = case Map.lookup x env of
  Just v -> v
  Nothing -> error $ "Variable not found: " ++ show x
eval _ (Const x) = x
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sin e) = sin (eval env e)

reverseDiff :: Expr -> Double -> (Tape, Double)
reverseDiff f x = (t, getDeriv t)
  where
    t = evalTape (Tape (eval (Map.singleton 1 x) f) empty) f
    empty = Map.empty

evalTape :: Tape -> Expr -> Tape
evalTape t (Var varId) = case Map.lookup varId (varTapes t) of
  Just t' -> t'
  Nothing -> error $ "Unbound variable " ++ show varId
evalTape t (Const c) = t
evalTape t (Add e1 e2) =
  let t1 = evalTape t e1
      t2 = evalTape t e2
      x1 = output t1
      x2 = output t2
   in t2 {output = x1 + x2}
evalTape t (Mul e1 e2) =
  let t1 = evalTape t e1
      t2 = evalTape t e2
      x1 = output t1
      x2 = output t2
   in t2 {output = x1 * x2}
evalTape t (Sin e) =
  let t' = evalTape t e
      x = output t'
      y = cos x
   in t' {output = sin x, varTapes = Map.map (scaleTape y) (varTapes t')}

getDeriv :: Tape -> Double
getDeriv t = case Map.elems (varTapes t) of
  [t'] -> output (evalTape t' (Const 1.0))
  _ -> error "Multiple variables found in tape"

scaleTape :: Double -> Tape -> Tape
scaleTape s t = t {output = s * output t, varTapes = Map.map (scaleTape s) (varTapes t)}
