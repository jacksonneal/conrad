module Matrix where

type Matrix = [[Double]]

shape :: Matrix -> (Int, Int)
shape m = (length m, length $ head m)

ones :: (Int, Int) -> Matrix
ones (r, c) = replicate r (replicate c 1.0)

cmap :: (Double -> Double) -> Matrix -> Matrix
cmap f = map (map f)

mSin :: Matrix -> Matrix
mSin = cmap sin

mCos :: Matrix -> Matrix
mCos = cmap cos

mMul :: Matrix -> Matrix -> Matrix
mMul = zipWith (zipWith (*))

mAdd :: Matrix -> Matrix -> Matrix
mAdd = zipWith (zipWith (+))

sMul :: Double -> Matrix -> Matrix
sMul s = cmap (* s)

square :: Matrix -> Matrix
square = cmap (^ 2)
