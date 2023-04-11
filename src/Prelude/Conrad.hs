{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Prelude.Conrad where

import Data.Function (on)
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

-- class Differentiable a where
--   dual :: a -> Dual a
--
-- instance Floating a => Differentiable (Scalar a) where
--   dual x = Dual x 1.0

diffF :: Floating a => (Dual a -> Dual b) -> a -> b
diffF f = tangent . f . (`Dual` 1.0)

gradF :: Floating a => ([Dual a] -> Dual b) -> [a] -> [b]
gradF f xs = map partial idxs
  where
    idxs = [0 .. length xs - 1]
    partial = tangent . f . oneHotDuals
    oneHotDuals i = mapIdx (\x j -> if i == j then Dual x 1.0 else Dual x 0.0) xs

tanhDemo :: Floating a => Scalar a -> Scalar a
tanhDemo (Scalar x) = Scalar ((1.0 - y) / (1.0 + y))
  where
    y = x ^ (-2)

tanhDemoV :: Floating a => a -> a
tanhDemoV x = (1.0 - y) / (1.0 + y)
  where
    y = x ^ (-2)

diffTanhDemo :: Floating a => a -> a
diffTanhDemo = diffF tanh

fn :: Floating a => [a] -> a
fn [x, y] = (sin (x / y) + x / y - exp y) * (x / y - exp y)

fn' :: Floating a => [a] -> [a]
fn' = gradF fn

-- data Op a b where
--   Add :: Num a => Op (a, a) a
--   Mul :: Num a => Op (a, a) a
--   Dot :: Num a => Op (Vector n a, Vector n a) a
--   Transpose :: Matrix m n a -> Op (Matrix m n a) (Matrix n m a)
--
-- data Value a where
--   VScalar :: Scalar a -> Value a
--   VVector :: Vector n (Value a) -> Value (Vector n a)
--   MatrixVal :: Matrix m n (Value a) -> Value (Matrix m n a)
--
-- data Tape a where
--   Nil :: Tape a
--   Cons :: Tape a -> Op (Rev a) -> Value a -> Tape (Rev a)
