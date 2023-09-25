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
    [ bench "sample" (whnf (sample 10) (list, Random.mkStdGen 0)),
      bench "shuffle" (whnf shuffle (list, Random.mkStdGen 0))
    ]

sample :: Int -> ([Int], Random.StdGen) -> ()
sample n (list, gen) =
  deepseq (fst (List.sample n list gen)) ()

shuffle :: ([Int], Random.StdGen) -> ()
shuffle (list, gen) =
  deepseq (fst (List.shuffle list gen)) ()
