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

perceptron :: Activation
perceptron x
  | x <= 0 = 0
  | otherwise = 1

sigmoid :: Activation
sigmoid z = 1 / (1 + exp (-z))

-- util

fmapNested :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmapNested = fmap . fmap

-- layers

fcl :: [[Float]] -> [[Float]] -> Activation -> [[Float]]
fcl = (flip fmapNested .) . mm

-- cost functions

mse :: [Float] -> [Float] -> Float
mse ys as = (1 / (2 * n)) * sse
  where
    n = fromIntegral $ length ys
    sse = sum $ zipWith (\y a -> (y - a) ^ 2) ys as
