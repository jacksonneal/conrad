{-# LANGUAGE GADTs #-}

module Prelude.Continuation where

import Data.Maybe (fromMaybe)
import Numeric.LinearAlgebra

data Expr a where
  Lit :: a -> Expr a
  Var :: String -> Expr a
  Add :: Expr a -> Expr a -> Expr a
  Mul :: Expr a -> Expr a -> Expr a
  Neg :: Expr a -> Expr a
  Inv :: Expr b -> Expr b
  Sin :: Expr b -> Expr b
  Cos :: Expr b -> Expr b
  Vector :: [Expr a] -> Expr (Vector a)

-- Matrix :: [[Expr a]] -> Expr (Matrix a)
-- Dot :: Expr (Vector b) -> Expr (Vector b) -> Expr b

instance Num a => Num (Expr a) where
  (+) = Add
  (*) = Mul
  negate = Neg
  abs = undefined
  signum = undefined
  fromInteger = Lit . fromInteger

instance Fractional a => Fractional (Expr a) where
  (/) = Mul . Inv
  fromRational = Lit . fromRational

instance Floating a => Floating (Expr a) where
  sin = Sin
  cos = Cos
  pi = Lit pi
  exp = undefined
  log = undefined
  asin = undefined
  acos = undefined
  atan = undefined
  sinh = undefined
  cosh = undefined
  asinh = undefined
  acosh = undefined
  atanh = undefined

diff :: Num a => Expr a -> (Expr a -> r) -> r
diff (Var x) k = k (Lit 1)
diff (Lit _) k = k (Lit 0)
diff (Add f g) k = diff f (\df -> diff g (k . Add df))
diff (Mul f g) k = diff f (\df -> diff g (k . Add (Mul df g) . Mul f))
diff (Sin f) k = diff f (k . Mul (Cos f))
diff (Cos f) k = diff f (k . Neg . Mul (Sin f))
diff (Vector xs) k = Vector (map (\x -> diff x k) xs)

-- diff (Matrix m) k = let diffs = map (map (diff k)) m in Matrix diffs
-- diff (Dot f g) k = undefined

-- diff (Dot f g) k = diff f (\df -> diff g (k . Add (Mul df g) . Mul f))
--
-- eval :: Field a => Expr a -> [(String, a)] -> a
-- eval (Lit n) _ = n
-- eval (Var x) env = fromMaybe (error $ "Variable not found: " ++ x) $ lookup x env
-- eval (Add f g) env = eval f env + eval g env
-- eval (Mul f g) env = eval f env * eval g env
-- eval (Sin f) env = sin $ eval f env
-- eval (Cos f) env = cos $ eval f env
-- eval (Dot f g) env = (eval f env) `dot` (eval g env)
--
-- df :: Field a => (Expr a -> Expr a) -> (a -> a)
-- df f x = eval dfExp [("x", x)]
--   where
--     fExp = f (Var "x")
--     dfExp = diff fExp id

-- dfv :: (Vector Double -> Double) -> Vector Double -> Vector Double
-- dfv f v = toColumns $ eval dfExp [("x", v)]
--   where
--     fExp = f (fromColumns [vector [Var "x", Var "y"]])
--     dfExp = diff fExp id

-- f :: Num a => a -> a
-- f x = x * x + x
--
-- f' = df f
