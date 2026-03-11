module Main (main) where

import Data.Vec.Lazy(Vec(..))

import StackMachine

main :: IO ()
main = print $ exec fib (6 ::: VNil)
