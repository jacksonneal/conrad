module Neuron where

-- linear algebra

dot :: Num a => [a] -> [a] -> a
dot = (sum .) . zipWith (*)

t :: [[a]] -> [[a]]
t ([] : _) = []
t x = map head x : t (map tail x)

vm :: Num a => [a] -> [[a]] -> [a]
vm = (. t) . map . dot

mv :: Num a => [[a]] -> [a] -> [a]
mv = flip (map . flip dot)

mm :: Num a => [[a]] -> [[a]] -> [[a]]
mm = (. t) . map . mv

-- activation

type Activation = Float -> Float

sigmoid :: Activation
sigmoid z = 1 / (1 + exp (-z))

sigmoid' :: Activation
sigmoid' x = sigmoid x * (1 - sigmoid x)

relu :: Activation
relu = max 0

relu' :: Activation
relu' x
  | x <= 0 = 0
  | otherwise = 1

-- layers

fcl :: [[Float]] -> [[Float]] -> Activation -> [[Float]]
fcl = (flip (map . map) .) . mm

-- cost functions

mse :: [Float] -> [Float] -> Float
mse ys as = (1 / (2 * n)) * sse
  where
    n = fromIntegral $ length ys
    sse = sum $ zipWith (\y a -> (y - a) ^ 2) ys as

mse' :: [Float] -> [Float] -> Float
mse' ys as = (1 / n) * se
  where
    n = fromIntegral $ length ys
    se = sum $ zipWith (-) ys as
