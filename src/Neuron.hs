module Neuron where

dot :: [Float] -> [Float] -> Float
dot xs ws = sum $ zipWith (*) xs ws

perceptron :: [Float] -> [Float] -> Float -> Int
perceptron xs ws b
  | s + b <= 0 = 0
  | otherwise = 1
  where
    s = dot xs ws

sigmoid :: [Float] -> [Float] -> Float -> Float
sigmoid xs ws b = 1 / (1 + exp (-s - b))
  where
    s = dot xs ws

mse :: [Float] -> [Float] -> Float
mse ys as = (1 / (2 * n)) * sse
  where
    n = fromIntegral $ length ys
    sse = sum $ zipWith (\y a -> (y - a) ^ 2) ys as
