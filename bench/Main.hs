module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import List.Shuffle qualified as List
import System.Random qualified as Random
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let list = [1 .. 1000] :: [Int]
  _ <- evaluate (force list)

  defaultMain
    [ bench "shuffle" (whnf (last . fst . List.shuffle list) (Random.mkStdGen 0))
    ]
