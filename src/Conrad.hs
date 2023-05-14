module Conrad where

import DiffFunc
import Matrix

fB :: Matrix -> Sin (Cos Var)
fB = Sin . Cos . Var

f :: Matrix -> Matrix
f = eval . fB

f' :: Matrix -> Matrix
f' = derivative . fB
