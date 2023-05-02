{-# LANGUAGE RankNTypes #-}

module Prelude.Composer where

type R a = forall r. (a -> Double -> r) -> r

-- The `Var` type represents variables in our expression
newtype Var = Var Int

-- The `Expr` type represents an expression in our language
data Expr
  = Const Double
  | EVar Int
  | Add Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr

-- The `diff` function computes the derivative of an expression with respect to a variable
diff :: Expr -> Var -> R Expr
diff (Const c) _ = \k -> k (Const 0) 0
diff (EVar v) (Var v') = \k -> k (Const $ if v == v' then 1 else 0) 0
diff (Add e1 e2) v = \k ->
  diff e1 v $ \de1 dv1 ->
    diff e2 v $ \de2 dv2 ->
      k (Add de1 de2) (dv1 + dv2)
diff (Mul e1 e2) v = \k ->
  diff e1 v $ \de1 dv1 ->
    diff e2 v $ \de2 dv2 ->
      k (Add (Mul de1 e2) (Mul e1 de2)) (dv1 * eval e2 + dv2 * eval e1)
diff (Sin e) v = \k ->
  diff e v $ \de dv ->
    k (Mul (Cos e) de) (dv * cos (eval e))
diff (Cos e) v = \k ->
  diff e v $ \de dv ->
    k (Mul (Const (-1)) (Mul (Sin e) de)) (dv * (-sin (eval e)))

-- The `eval` function evaluates an expression
eval :: Expr -> Double
eval (Const c) = c
eval (EVar _) = error "Cannot evaluate expression containing variables"
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Sin e) = sin (eval e)
eval (Cos e) = cos (eval e)

-- A test expression
testExpr :: Expr
testExpr = (Mul (Add (EVar 1)) (EVar 2)) (Sin (Var 2))

-- Computing the derivative of the test expression with respect to `Var 1`
testDeriv :: Expr
testDeriv = fst $ diff testExpr (Var 1) (\de dv -> (de, dv))

-- Evaluating the derivative of the test expression with respect to `Var 1` at a particular point
testDerivValue :: Double
testDerivValue = eval $ testDeriv `Mul` Const 2 -- multiplying by 2 because we took the derivative with respect to `Var 1` instead of `Var 2`
