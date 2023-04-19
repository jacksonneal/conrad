{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (cos, sin)

class Tape t where
  type Var t :: *
  var :: Double -> t (Var t)
  sin :: t a -> t a
  cos :: t a -> t a
  add :: t a -> t a -> t a
  mul :: t a -> t a -> t a
  grad :: t a -> t (Var t) -> Double

data Expr t a where
  Var :: Var t -> Expr t (Var t)
  Sin :: Expr t a -> Expr t a
  Cos :: Expr t a -> Expr t a
  Add :: Expr t a -> Expr t a -> Expr t a
  Mul :: Expr t a -> Expr t a -> Expr t a

newtype R a = R (forall t. Tape t => Expr t a)

instance Tape R where
  type Var R = Int
  var x = R (Var 0)
  sin (R e) = R (Sin e)
  cos (R e) = R (Cos e)
  add (R e1) (R e2) = R (Add e1 e2)
  mul (R e1) (R e2) = R (Mul e1 e2)
  grad (R e) (R v) = grad' e v 1.0
    where
      grad' (Var x) v w
        | x == v = w
        | otherwise = 0.0
      grad' (Sin e) v w = cos (eval e) * grad' e v w
      grad' (Cos e) v w = -sin (eval e) * grad' e v w
      grad' (Add e1 e2) v w = grad' e1 v w + grad' e2 v w
      grad' (Mul e1 e2) v w = grad' e1 v (eval e2 * w) + grad' e2 v (eval e1 * w)

eval :: Expr t a -> R a
eval = R

example :: R (Var R)
example = do
  x <- var 0.5
  y <- var 4.2
  z <- x `mul` y `add` sin x
  return x

main :: IO ()
main = print (grad example (eval example))
