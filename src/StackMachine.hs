{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module StackMachine where

import Data.Bits ((.&.))
import Data.List (uncons)

import Data.Foldable (foldlM)

import Control.Monad.Identity (Identity)

import Data.Fin (Fin, fin0, fin1, fin2, fin3, fin4)
import Data.Type.Nat (Nat (S), Nat1, Nat5)
import Data.Vec.Lazy (Vec (..), (!))

data Instr = And | Add | Eq | Dup | Swap | Push Int | Pop
  deriving (Show)

{- | list of continuation blocks to execute. Execute the first, and when it returns,
execute the next. @[]@ returns.
-}
data Cont n = Uncond [Fin n] | Cond [Fin n] [Fin n]

data Block n
  = Block
  { instrs :: [Instr]
  , cont :: Cont n
  }

data State n
  = State
  { callStack :: [Fin n]
  , valStack :: [Int]
  }
  deriving (Show)

type Program n = Vec n (Block n)

exec :: forall m n. (Machine m) => Program (S n) -> [Int] -> m [Int]
exec code vals = go fin0 (State{callStack = [], valStack = vals})
 where
  go :: Fin (S n) -> State (S n) -> m [Int]
  go i s =
    execBlock (code ! i) s >>= \case
      Left (s', i') -> go i' s'
      Right vs -> result vs

execBlock ::
  (Machine m) => Block n -> State n -> m (Either (State n, Fin n) [Int])
execBlock Block{..} s0 = do
  newStack <- evalInstrs instrs (valStack s0)
  let (conts, valPopped) =
        case cont of
          Uncond t -> (t, newStack)
          Cond z nz ->
            case newStack of
              [] -> error "value stack underflow"
              (x : rest) -> (if x == 0 then z else nz, rest)
  pure $
    case uncons (conts ++ callStack s0) of
      Nothing -> Right valPopped
      Just (dest, rest) ->
        Left (State{valStack = valPopped, callStack = rest}, dest)

evalInstrs :: (Machine m) => [Instr] -> [Int] -> m [Int]
evalInstrs = flip (foldlM $ flip step)

eval1 :: Instr -> [Int] -> [Int]
eval1 Add = binOp (+)
eval1 And = binOp (.&.)
eval1 Eq = binOp $ \a b -> if a == b then 1 else 0
eval1 Dup = \case
  x : rest -> x : x : rest
  [] -> error "Value stack underflow"
eval1 Swap = \case
  (x : y : rest) -> y : x : rest
  _ -> error "Value stack underflow"
eval1 (Push x) = (x :)
eval1 Pop = \case
  _ : rest -> rest
  [] -> error "Value stack underflow"

binOp :: (Int -> Int -> Int) -> [Int] -> [Int]
binOp f (x : y : rest) = f x y : rest
binOp _ _ = error "Value stack underflow"

class (Monad m) => Machine m where
  step :: Instr -> [Int] -> m [Int]
  result :: (Show a) => a -> m a

instance Machine Identity where
  step i s = pure $ eval1 i s
  result = pure

instance Machine IO where
  step i s = do
    putStrLn $ concat [show s, " ", show i]
    _ <- getChar
    pure $ eval1 i s

  result a = do
    putStrLn $ "Result: " ++ show a
    pure a

twoAddTwo :: Program Nat1
twoAddTwo = Block [Push 2, Push 2, Add] (Uncond []) ::: VNil

{-

Nth Fibbonacci algorithm

Base case: F(n) = n, when n = 0 or n = 1
Recursive case: F(n) = F(n-1) + F(n-2) for n>1

-}

fib :: Program Nat5
fib =
  Block [Dup] (Cond [] [fin1]) -- 0
    ::: Block [Dup, Push 1, Eq] (Cond [fin2] []) -- 1
    ::: Block [Dup, Push (-1), Add] (Uncond [fin0, fin3]) -- 2
    ::: Block [Swap, Push (-2), Add] (Uncond [fin0, fin4]) -- 3
    ::: Block [Add] (Uncond []) -- 4
    ::: VNil
