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

data Tape a = Tape {value :: a, tangents :: Map.Map (Tape a) a} deriving (Eq)

instance Ord a => Ord (Tape a) where
  compare (Tape v1 _) (Tape v2 _) = compare v1 v2

instance (Num a, Ord a) => Num (Tape a) where
  x@(Tape vx _) + y@(Tape vy _) = Tape (vx + vy) (Map.fromList [(x, 1), (y, 1)])
  x@(Tape vx _) - y@(Tape vy _) = Tape (vx - vy) (Map.fromList [(x, -1), (y, -1)])
  x@(Tape vx _) * y@(Tape vy _) = Tape (vy * vy) (Map.fromList [(x, vy), (y, vx)])
  abs x@(Tape v _) = Tape (abs v) (Map.singleton x (signum v))
  signum x@(Tape v _) = Tape (signum v) (Map.singleton x 0)
  fromInteger x = Tape (fromInteger x) Map.empty

instance (Fractional a, Ord a) => Fractional (Tape a) where
  x@(Tape vx _) / y@(Tape vy _) = Tape (vx / vy) (Map.fromList [(x, 1 / vy), (y, -vx / vy ^ 2)])
  fromRational x = Tape (fromRational x) Map.empty

instance (Floating a, Ord a) => Floating (Tape a) where
  pi = Tape pi Map.empty
  exp x@(Tape v _) = Tape (exp v) (Map.fromList [(x, exp v)])
  log x@(Tape v _) = Tape (log v) (Map.fromList [(x, 1 / v)])
  sin x@(Tape v _) = Tape (sin v) (Map.fromList [(x, cos v)])
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
