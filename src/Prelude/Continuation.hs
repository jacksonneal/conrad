module Prelude.Continuation where

-- A simple variable type with a Fractional instance
newtype Var a = Var a

instance Fractional a => Num (Var a) where
  Var x + Var y = Var (x + y)
  Var x - Var y = Var (x - y)
  Var x * Var y = Var (x * y)
  fromInteger x = Var (fromInteger x)
  negate (Var x) = Var (negate x)
  abs (Var x) = Var (abs x)
  signum (Var x) = Var (signum x)

instance Fractional a => Fractional (Var a) where
  Var x / Var y = Var (x / y)
  fromRational x = Var (fromRational x)

-- The Diff type represents a value and its derivative
data Diff a = Diff a (Diff a -> Diff a)

-- The Num instance for Diff allows us to define functions
-- on Diff values and take their derivatives using reverse-mode AD
instance Num a => Num (Diff a) where
  Diff x dx + Diff y dy = Diff (x + y) (\d -> dx d + dy d)
  Diff x dx - Diff y dy = Diff (x - y) (\d -> dx d - dy d)
  Diff x dx * Diff y dy = Diff (x * y) (\d -> dx d * y + x * dy d)
  fromInteger x = Diff (fromInteger x) (\_ -> fromInteger 0)
  negate (Diff x dx) = Diff (negate x) (\d -> negate (dx d))
  abs (Diff x dx) = Diff (abs x) (\d -> if x >= 0 then dx d else negate (dx d))
  signum (Diff x dx) = Diff (signum x) (\_ -> fromInteger 0)

-- The grad function computes the gradient of a function
grad :: (Fractional a, Fractional b) => (Diff a -> Diff b) -> a -> b
grad f x = snd $ runCont (f (Diff x (const 1))) id

-- The cpsReverse function computes the derivative of a function using continuation passing
cpsReverse :: (Fractional a, Fractional b) => (Diff a -> Diff b) -> a -> b
cpsReverse f x = runCont (f (Diff x (const 1))) (\(Diff _ dx) -> dx (Diff 1 (const 0)))

-- The Cont type is a continuation monad transformer
type ContT r m a = (a -> m r) -> m r

-- The evalCont function runs a computation in the ContT monad transformer
evalContT :: Monad m => ContT r m r -> m r
evalContT m = m return

-- The bindCont function is a helper function for composing continuation-passing functions
bindCont :: Monad m => ContT a m b -> (a -> ContT b m c) -> ContT c m b
bindCont m f k = m (\a -> f a k)

-- The Cont type is a continuation monad
newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap f m = Cont (\k -> runCont m (k . f))

instance Applicative (Cont r) where
  pure x = Cont (\k -> k x)
  f <*> x = Cont (\k -> runCont f (\g -> runCont x (k . g)))

instance Monad (Cont r) where
  return x = Cont (\k -> k x)
  m >>= f = Cont (\k -> runCont m (\x -> runCont (f x) k))
