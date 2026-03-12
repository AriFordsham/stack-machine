module Main (main) where

import StackMachine

import Control.Monad.Identity

main :: IO ()
main = mapM_ (\i -> putStrLn $ concat ["fib ", show i, ": ", show (runIdentity $ exec fib [i])]) [0..10]
-- main = do
--   _ <- exec fib [2]
--   pure ()
