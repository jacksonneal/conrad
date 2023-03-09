module Neuron where

dot :: Num a => [a] -> [a] -> a
dot = (sum .) . zipWith (*)

t :: [[a]] -> [[a]]
t x = map head x : t (map tail x)

vm :: Num a => [a] -> [[a]] -> [a]
vm = (. t) . map . dot

mv :: Num a => [[a]] -> [a] -> [a]
mv = flip (map . flip dot)

mm :: Num a => [[a]] -> [[a]] -> [[a]]
mm = (. t) . map . mv

perceptron :: (Ord a, Num a) => a -> Int
perceptron x
  | x <= 0 = 0
  | otherwise = 1

sigmoid :: Float -> Float
sigmoid z = 1 / (1 + exp (-z))

ffl :: [[Float]] -> [[Float]] -> [Float] -> [[Float]]
ffl xss wss bs = map (\xs -> zipWith (sigmoid xs) wss bs) xss

ffn :: [[Float]] -> [[[Float]]] -> [[Float]] -> [[Float]]
ffn xss wsss bss = foldr (\(wss, bs) xss -> xss) xss (zip wsss bss)

mse :: [Float] -> [Float] -> Float
mse ys as = (1 / (2 * n)) * sse
  where
    n = fromIntegral $ length ys
    sse = sum $ zipWith (\y a -> (y - a) ^ 2) ys as
