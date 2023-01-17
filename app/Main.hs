{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Data.List.Split
import Text.Printf

type Vector = [Double]

type Matrix = [Vector]

numRows :: Matrix -> Int
numRows = length

numCols :: Matrix -> Int
numCols = length . head

scalarVectorProduct :: Double -> Vector -> Vector
scalarVectorProduct n v = [n * x | x <- v]

scalarMatrixProduct :: Double -> Matrix -> Matrix
scalarMatrixProduct n m = [scalarVectorProduct n r | r <- m]

vectorSum :: Vector -> Vector -> Vector
vectorSum = zipWith (+)

negV :: Vector -> Vector
negV = map negate

transpose :: Matrix -> Matrix
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)

dotProduct :: Vector -> Vector -> Double
dotProduct = (sum .) . zipWith (*)

matrixVectorProduct :: Matrix -> Vector -> Vector
matrixVectorProduct m v = [dotProduct r v | r <- m]

matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n = [map (dotProduct r) (transpose n) | r <- m]

diagonal :: Matrix -> Vector
diagonal m = zipWith (!!) m [0 ..]

cut :: [a] -> Int -> [a]
cut [] _ = []
cut v n = take (n - 1) v ++ drop n v

remove :: Matrix -> Int -> Int -> Matrix
remove m i j = transpose $ cut (transpose $ cut m i) j

determinant :: Matrix -> Double
determinant [[n]] = n
determinant m =
  sum
    [ (-1) ^ (j + 1) * head m !! (j - 1) * determinant (remove m 1 j)
      | j <- [1 .. numCols m]
    ]

cofactor :: Matrix -> Int -> Int -> Double
cofactor m i j = (-1.0) ^ (i + j) * determinant (remove m i j)

cofactorMatrix :: Matrix -> Matrix
cofactorMatrix m = [[cofactor m i j | j <- [1 .. n]] | i <- [1 .. n]]
  where
    n = length m

inverse :: Matrix -> Matrix
inverse m = transpose [[x / determinant m | x <- cofm] | cofm <- cofactorMatrix m]

mean :: Vector -> Double
mean v = sum v / fromIntegral (length v)

variance :: Vector -> Double
variance v = sum (map (^ 2) (map (subtract (mean v)) v)) / fromIntegral (length v - 1)

x :: Matrix
x =
  [ [1, -0.6264538, -0.8204684],
    [1, 0.1836433, 0.4874291],
    [1, -0.8356286, 0.7383247],
    [1, 1.5952808, 0.5757814],
    [1, 0.3295078, -0.3053884]
  ]

y :: Vector
y = [0.06485897, 1.06091561, -0.71854449, -0.04363773, 1.14905030]

main :: IO ()
main =
  let n = numRows x

      p = numCols x - 1

      -- inverse of XX' needed for the hat matrix
      inverse_X_X' = inverse $ matrixProduct (transpose x) x

      -- the hat hatrix
      hat = matrixProduct inverse_X_X' (transpose x)

      -- the regression coefficient estimates
      betas = matrixVectorProduct hat y

      -- fitted values
      fitted = matrixVectorProduct x betas

      -- residuals
      res = vectorSum y $ negV fitted

      -- Total sum of squares
      ssto = sum $ map (^ 2) $ map (subtract $ mean y) y

      -- Sum of squared errors
      sse = sum $ map (^ 2) res

      -- Regression sum of squares
      ssr = ssto - sse

      -- mean squared error
      mse = sse / fromIntegral (n - p - 1)

      -- regression mean square
      msr = ssr / fromIntegral p

      -- F statistic for the regression
      f = msr / mse

      -- on p and n-p-1 degrees of freedom

      -- standard error of regression coefficients
      se_coef = map sqrt $ diagonal $ scalarMatrixProduct mse inverse_X_X'

      -- r-squared
      r2 = 1 - (sse / ssto)

      -- adjusted r-squared
      r2_adj = 1 - (mse / variance y)

      -- helper function for output
      vector_to_string :: Vector -> String
      vector_to_string xs = unwords $ printf "%.3f" <$> xs
   in putStr
        ( "Estimates:   "
            ++ vector_to_string betas
            ++ "\n"
            ++ "Std. Error:  "
            ++ vector_to_string se_coef
            ++ "\n"
            ++ "R-squared:   "
            ++ printf "%.3f" r2
            ++ "  Adj R-sq:  "
            ++ printf "%.3f" r2_adj
            ++ "\n"
            ++ "F Statistic: "
            ++ printf "%.3f" f
            ++ " on "
            ++ show p
            ++ " and "
            ++ show (n - p - 1)
            ++ " degrees of freedom"
            ++ "\n"
        )

-- ddb :: Matrix -> Vector -> Vector -> Double -> Double
-- ddb x y w b = 0
--
-- ddw :: Matrix -> Vector -> Vector -> Float -> Vector
-- ddw x y w b = []
--
-- updateBias :: Double -> Int -> Matrix -> Vector -> Vector -> Double -> Double
-- updateBias a m x y w b = b - (a / fromIntegral m) * ddb x y w b
--
-- updateWeights :: Double -> Int -> Matrix -> Vector -> Vector -> Float -> Vector
-- updateWeights a m x y w b = w - (a / fromIntegral m) `vectorScalarProduct` ddw x y w b
--
-- trainHelp :: Double -> Int -> Int -> Matrix -> Vector -> Vector -> Float -> (Vector, Float)
-- trainHelp a i m x y w b
--   | i == 0 = (w, b)
--   | otherwise =
--       trainHelp
--         a
--         (i - 1)
--         m
--         x
--         y
--         (updateWeights a m x y w b)
--         (updateBias a m x y w b)
--
-- train :: Float -> Int -> Matrix -> Vector -> (Vector, Float)
-- train a i x y =
--   let m = numRows x
--       n = numCols x
--       w = [0 | _ <- [0 .. n]]
--       b = 0
--    in trainHelp a i m x y w b
--
-- splitFeatureLabel :: Matrix -> (Matrix, Vector)
-- splitFeatureLabel rs = ([init r | r <- rs], [last r | r <- rs])
--
-- readRecord :: String -> [Float]
-- readRecord = map read . splitOn ","
--
-- readInput :: String -> IO [[Float]]
-- readInput filePath = do
--   contents <- readFile filePath
--   let records = map readRecord $ lines contents
--   return records
--
-- inputFilePath = "./salary_data.csv"

-- main :: IO ()
-- main = do
--   putStrLn "hello"
--   lm
