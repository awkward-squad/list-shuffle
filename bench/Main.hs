module Main (main) where

import Control.DeepSeq (deepseq, force)
import Control.Exception (evaluate)
import List.Shuffle qualified as List
import System.Random qualified as Random
import Test.Tasty.Bench (bench, defaultMain, whnf)

main :: IO ()
main = do
  let list = [1 .. 1000000] :: [Int]
  _ <- evaluate (force list)

  defaultMain
    [ bench "shuffle" (whnf shuffle (list, Random.mkStdGen 0))
    ]

shuffle :: ([Int], Random.StdGen) -> ()
shuffle (list, gen) =
  deepseq (fst (List.shuffle list gen)) ()
