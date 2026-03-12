{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module StackMachine where

import Data.Bits ((.&.))
import Data.List (uncons, (!?))

import Data.Foldable (foldlM)

import Control.Monad.Identity

data Instr = And | Add | Eq | Dup | Swap | Push Int | Pop
  deriving (Show)

{- | list of continuation blocks to execute. Execute the first, and when it returns,
execute the next. @[]@ returns.
-}
data Cont = Uncond [Int] | Cond [Int] [Int]

data Block
  = Block
  { instrs :: [Instr]
  , cont :: Cont
  }

data State
  = State
  { callStack :: [Int]
  , valStack :: [Int]
  }
  deriving (Show)

exec :: forall m. (Machine m) => [Block] -> [Int] -> m [Int]
exec code vals = go 0 (State{callStack = [], valStack = vals})
 where
  go :: Int -> State -> m [Int]
  go i s = case code !? i of
    Nothing -> error "reference out of bounds"
    Just b ->
      execBlock b s >>= \case
        Left (s', i') -> go i' s'
        Right vs -> result vs

execBlock :: (Machine m) => Block -> State -> m (Either (State, Int) [Int])
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

twoAddTwo :: [Block]
twoAddTwo = [Block [Push 2, Push 2, Add] (Uncond [])]

{-

Nth Fibbonacci algorithm

Base case: F(n) = n, when n = 0 or n = 1
Recursive case: F(n) = F(n-1) + F(n-2) for n>1

-}

fib :: [Block]
fib =
  [ Block [Dup] (Cond [] [1]) -- 0
  , Block [Dup, Push 1, Eq] (Cond [2] []) -- 1
  , Block [Dup, Push (-1), Add] (Uncond [0, 3]) -- 2
  , Block [Swap, Push (-2), Add] (Uncond [0, 4]) -- 3
  , Block [Add, Push 0] (Uncond []) -- 4
  ]
