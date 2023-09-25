module List.Shuffle
  ( shuffle,
    shuffle_,
    sample,
  )
where

import Control.Monad.ST (runST)
import Control.Monad.ST.Strict (ST)
import Data.Foldable qualified as Foldable
import Data.Primitive.Array qualified as Array
import System.Random (RandomGen)
import System.Random qualified as Random

-- | Shuffle a list.
--
-- This function can be adapted to work in a couple common situations:
--
-- === __A state monad__
--
-- You are working in a state monad with access to a pseudo-random number generator somewhere in the state type. You
-- also have a lens onto this field, which is commonly either provided by @generic-lens@/@optics@ or written manually:
--
-- > import System.Random qualified as Random
-- >
-- > data MyState = MyState
-- >   { ...
-- >   , prng :: Random.StdGen
-- >   , ...
-- >   }
-- >
-- > prngLens :: Lens' MyState Random.StdGen
--
-- In this case, you can adapt 'shuffle' to work in your monad as follows:
--
-- > import Control.Lens qualified as Lens
-- > import Control.Monad.Trans.State.Strict qualified as State
-- > import List.Shuffle qualified as List
-- >
-- > shuffleList :: Monad m => [a] -> StateT MyState m [a]
-- > shuffleList =
-- >   Lens.zoom prngLens . State.state . List.shuffle
--
-- === __A reader monad__
--
-- You are working in a reader monad, with access to a pseudo-random number generator somewhere in the environment,
-- in a mutable cell like an @IORef@ or @TVar@:
--
-- > import System.Random qualified as Random
-- >
-- > data MyMonad a
-- >
-- > instance MonadReader MyEnv MyMonad
-- >
-- > data MyEnv = MyEnv
-- >   { ...
-- >   , prngRef :: IORef Random.StdGen
-- >   , ...
-- >   }
--
-- In this case, you can adapt 'shuffle' to work in your monad as follows:
--
-- > import Data.IORef
-- > import List.Shuffle qualified as List
-- > import System.Random qualified as Random
-- >
-- > shuffleList :: [a] -> MyMonad [a]
-- > shuffleList list = do
-- >   Env {prngRef} <- ask
-- >   gen <- atomicModifyIORef' prngRef Random.split
-- >   pure (List.shuffle_ list gen)
--
-- === __Some IO monad in which the global generator is fine__
--
-- You are working in some IO monad, and you just want to get shuffling as quickly as possible.
--
-- In this case, you can adapt 'shuffle' to work in your monad as follows:
--
-- > import List.Shuffle qualified as List
-- > import System.Random qualified as Random
-- >
-- > shuffleList :: MonadIO m => [a] -> m [a]
-- > shuffleList list =
-- >   List.shuffle_ list <$> Random.newStdGen
shuffle :: (RandomGen g) => [a] -> g -> ([a], g)
shuffle list gen0 =
  runST do
    array <- listToMutableArray list
    gen1 <- shuffleN (Array.sizeofMutableArray array - 1) array gen0
    array1 <- Array.unsafeFreezeArray array
    pure (Foldable.toList array1, gen1)
{-# SPECIALIZE shuffle :: [a] -> Random.StdGen -> ([a], Random.StdGen) #-}

-- `shuffleN n array g` shuffles the first `n` elements of `array`, i.e. it performs the Fisher-Yates algorithm, but
-- stopping after `n` elements, effectively leaving those `n` elements at the head of the array "shuffled" and the rest
-- in some random indeterminate order.
--
-- Call `len` the length of the array minus 1. When `n` is the len, the whole array gets shuffled, as shuffling `n-1` of
-- `n` elements is equivalent to shuffling all `n` elements.
--
-- It's fine to pass nonsense values for `n` - negative numbers are equivalent to 0, and numbers larger than `len` are
-- equivalent to `len`.
shuffleN :: (RandomGen g) => Int -> Array.MutableArray s a -> g -> ST s g
shuffleN n0 array =
  go 0
  where
    go !i gen0
      | i >= n = pure gen0
      | otherwise = do
          let (j, gen1) = Random.uniformR (i, m) gen0
          swapArray i j array
          go (i + 1) gen1

    n = min n0 m
    m = Array.sizeofMutableArray array - 1

-- | Like 'shuffle', but discards the final generator.
shuffle_ :: (RandomGen g) => [a] -> g -> [a]
shuffle_ list g =
  fst (shuffle list g)
{-# SPECIALIZE shuffle_ :: [a] -> Random.StdGen -> [a] #-}

-- | Sample (without replacement) @n@ elements of a list.
--
-- @sample n xs@ is equal to taking @n@ elements from the result of @shuffle n xs@, but can be noticeably more efficient
-- if @n@ is sufficiently smaller than the length of @xs@. Always benchmark! :)
sample :: (RandomGen g) => Int -> [a] -> g -> ([a], g)
sample n list gen0 =
  runST do
    array <- listToMutableArray list
    gen1 <- shuffleN n array gen0
    array1 <- Array.unsafeFreezeArray array
    pure (take n (Foldable.toList array1), gen1)

-- Swap two elements in a mutable array.
swapArray :: Int -> Int -> Array.MutableArray s a -> ST s ()
swapArray i j array = do
  xi <- Array.readArray array i
  xj <- Array.readArray array j
  Array.writeArray array i xj
  Array.writeArray array j xi
{-# INLINE swapArray #-}

-- Construct a mutable array from a list.
listToMutableArray :: [a] -> ST s (Array.MutableArray s a)
listToMutableArray list = do
  array <- Array.newArray (length list) undefined
  let writeElems !i = \case
        [] -> pure ()
        x : xs -> do
          Array.writeArray array i x
          writeElems (i + 1) xs
  writeElems 0 list
  pure array
{-# INLINE listToMutableArray #-}
