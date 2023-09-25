module List.Shuffle
  ( shuffle,
    shuffle_,
  )
where

import Control.Monad.ST (runST)
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
    array <- Array.newArray len undefined
    let writeElems !i = \case
          [] -> pure ()
          x : xs -> do
            Array.writeArray array i x
            writeElems (i + 1) xs
    writeElems 0 list
    let swapElems !i gen
          | i >= len - 1 = pure gen
          | otherwise = do
              let (j, gen1) = Random.uniformR (i, len - 1) gen
              xi <- Array.readArray array i
              xj <- Array.readArray array j
              Array.writeArray array i xj
              Array.writeArray array j xi
              swapElems (i + 1) gen1
    gen1 <- swapElems 0 gen0
    array1 <- Array.unsafeFreezeArray array
    pure (Foldable.toList array1, gen1)
  where
    len = length list
{-# SPECIALIZE shuffle :: [a] -> Random.StdGen -> ([a], Random.StdGen) #-}

-- | Like 'shuffle', but discards the final generator.
shuffle_ :: (RandomGen g) => [a] -> g -> [a]
shuffle_ list g =
  fst (shuffle list g)
{-# SPECIALIZE shuffle_ :: [a] -> Random.StdGen -> [a] #-}
