{-# LANGUAGE BlockArguments #-}

import Control.Monad
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- | The 'Tape' data type represents a tape of operations.
-- It is used to keep track of the computation graph and perform automatic differentiation.
newtype Tape s = Tape {nodes :: VM.STVector s Node}

-- | The 'Var' data type represents a variable in the computation graph.
data Var s = Var {tape :: Tape s, index :: Int, value :: Double}

-- | The 'Grad' data type represents a vector of partial derivatives.
newtype Grad = Grad {derivs :: V.Vector Double}

-- | The 'Node' data type represents a node in the computation graph.
-- It contains the weights and dependencies of the node.
data Node = Node {weights :: (Double, Double), deps :: (Int, Int)}

-- | Create a new 'Tape'.
newTape :: ST s (Tape s)
newTape = Tape <$> VM.new 0

-- | Create a new 'Var' with a given value.
newVar :: Tape s -> Double -> ST s (Var s)
newVar t v = do
  let i = VM.length (nodes t)
  VM.grow (nodes t) 1
  let n = Node {weights = (0.0, 0.0), deps = (i, i)}
  VM.write (nodes t) i n
  return $ Var t i v

-- | Get the length of the 'Tape'.
tapeLength :: Tape s -> Int
tapeLength t = VM.length (nodes t)

-- | Append a new node with zero dependencies and weights to the 'Tape'.
push0 :: Tape s -> ST s Int
push0 t = do
  let weights' = (0.0, 0.0)
      deps' = (i, i)
      i = VM.length (nodes t)
  VM.grow (nodes t) 1
  let n = Node {weights = weights', deps = deps'}
  VM.write (nodes t) i n
  return i

-- | Append a new node with one dependency and weight to the 'Tape'.
push1 :: Tape s -> Int -> Double -> ST s Int
push1 t dep0 weight0 = do
  let weights' = (weight0, 0.0)
      i = VM.length (nodes t)
  VM.grow (nodes t) 1
  let n = Node {weights = weights', deps = (dep0, i)}
  VM.write (nodes t) i n
  return i

-- | Append a new node with two dependencies and weights to the 'Tape'.
push2 :: Tape s -> Int -> Double -> Int -> Double -> ST s Int
push2 t dep0 weight0 dep1 weight1 = do
  let weights' = (weight0, weight1)
      len = VM.length (nodes t)
  VM.grow (nodes t) 1
  let n = Node {weights = weights', deps = (dep0, dep1)}
  VM.write (nodes t) len n
  return len

-- | Compute the partial derivatives of a 'Var' with respect to all variables in the 'Tape'.
grad :: Var s -> ST s Grad
grad v@(Var t i _) = do
  let i = VM.length (nodes t)
  derivs <- VM.replicate (i - 1) 0.0
  let backprop i = do
        node <- VM.read (nodes t) i
        deriv <- VM.read derivs i
        let (d0, d1) = deps node
        VM.write derivs d0 0.0
        VM.write derivs d1 0.0
  forM_ (reverse [1 .. i - 1]) backprop
  frozenDerivs <- V.freeze derivs
  return Grad {derivs = frozenDerivs}

example :: IO ()
example = do
  let vec = runST $ do
        vecRef <- newSTRef []
        forM_ [1 .. 10] $ \i -> do
          vec <- readSTRef vecRef
          writeSTRef vecRef (vec ++ [i])
        readSTRef vecRef
  print vec
