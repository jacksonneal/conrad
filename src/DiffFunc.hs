module DiffFunc where

import Matrix

class DiffFunc f where
  eval :: f -> Matrix
  derivative :: f -> Matrix

newtype Var = Var {unVar :: Matrix}

instance DiffFunc Var where
  eval (Var x) = x
  derivative (Var x) = ones $ shape x

newtype Sin a = Sin a

instance (DiffFunc a) => DiffFunc (Sin a) where
  eval (Sin df) = mSin (eval df)
  derivative (Sin df) = mCos (derivative df)

newtype Cos a = Cos a

instance (DiffFunc a) => DiffFunc (Cos a) where
  eval (Cos df) = mCos (eval df)
  derivative (Cos df) = sMul (-1) (mSin $ derivative df)

newtype Mul a b = Mul (a, b)

instance (DiffFunc a, DiffFunc b) => DiffFunc (Mul a b) where
  eval (Mul (f, g)) = eval f `mMul` eval g
  derivative (Mul (f, g)) = eval f `mMul` derivative g `mAdd` derivative f `mMul` eval g
