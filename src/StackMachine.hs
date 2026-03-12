{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module StackMachine where

import Data.Bits ((.&.))
import Data.List ((!?))

import Data.Foldable (foldlM)

import Control.Monad.Identity

data Instr where
  And :: Instr
  Add :: Instr
  Eq :: Instr
  Dup :: Instr
  Swap :: Instr
  Push :: Int -> Instr
  Pop :: Instr
  deriving (Show)

type Cont = [Int]

data Block
  = Block
  { instrs :: [Instr]
  , onZero :: Cont
  , onNonZero :: Cont
  }

data State
  = State
  { callStack :: [Cont]
  , valStack :: [Int]
  }
  deriving (Show)

--OUT OF DATE: Jump semantics: At the end of a block, pop the top (head) values from the value stack
-- and the call stack. If the value is zero, jump to the first block
-- in the pair, otherwise jump to the second block. An empty call stack exits.
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
  pure $ case newStack of
    [] -> error "value stack underflow"
    x : valPopped -> do
      let j = if x == 0 then onZero else onNonZero
      case resolveCallStack (j:callStack s0) of
        Nothing -> Right valPopped
        Just (callPopped, dest) ->
          Left (State{valStack = valPopped, callStack = callPopped}, dest)

resolveCallStack :: [Cont] -> Maybe ([Cont], Int)
resolveCallStack ((dest:cont):stack) = Just (cont : stack, dest)
resolveCallStack ([]:rest) = resolveCallStack rest
resolveCallStack [] = Nothing 

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

twoAddTwo :: [[Instr]]
twoAddTwo = [[Push 2, Push 2, Add]]

{-

Nth Fibbonacci algorithm

Base case: F(n) = n, when n = 0 or n = 1
Recursive case: F(n) = F(n-1) + F(n-2) for n>1

-}

fib :: [Block]
fib =
  [ Block [Dup] [] [1] -- 0
  , Block [Dup, Push 1, Eq] [2] [] -- 1
  , Block [Dup, Push (-1), Add, Push 0] [0,3] undefined -- 2
  , Block [Swap, Push (-2), Add, Push 0]  [0,4] undefined -- 3
  , Block [Add, Push 0] [] undefined -- 4
  ]
