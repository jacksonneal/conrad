module Mnist where

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import System.Random

render :: Integral a => a -> Char
render n = s !! (fromIntegral n * length s `div` 256)
  where
    s = " .:oO@"

main :: IO ()
main = do
  s <- decompress <$> BS.readFile "data/train-images-idx3-ubyte.gz"
  l <- decompress <$> BS.readFile "data/train-labels-idx1-ubyte.gz"
  n <- (`mod` 60000) <$> randomIO
  putStr . unlines $
    [render . BS.index s . (n * 28 ^ 2 + 16 + r * 28 +) <$> [0 .. 27] | r <- [0 .. 27]]
  print $ BS.index l (n + 8)

gauss :: Float -> IO Float
gauss stdev = do
  x <- randomIO
  y <- randomIO
  return $ stdev * sqrt (-2 * log x) * cos (2 * pi * y)

relu :: Float -> Float
relu = max 0

relu' :: Float -> Float
relu' x
  | x < 0 = 0
  | otherwise = 1
