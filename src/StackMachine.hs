{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}

module StackMachine where

import Data.Bits((.&.))

import Data.Type.Nat
import Data.Vec.Lazy

data FreeCat k a b where
  End :: FreeCat k a a
  (:*) :: k a b -> FreeCat k b c -> FreeCat k a c

infixr 5 :*

(<:>) :: FreeCat k a b -> FreeCat k b c -> FreeCat k a c
End       <:> ys = ys
(x :* xs) <:> ys = x :* (xs <:> ys)

infixr 5 <:>

data Instr a b where
  And    ::                                           Instr ('S ('S n))   ('S n)
  Add    ::                                           Instr ('S ('S n))   ('S n)
  Eq     ::                                           Instr ('S ('S n))   ('S n)
  Dup    ::                                           Instr ('S n)        ('S ('S n))
  Swap   ::                                           Instr ('S ('S n))   ('S ('S n))
  Push   :: Int ->                                    Instr n             ('S n)
  Pop    ::                                           Instr ('S n)        n
  Branch :: FreeCat Instr a b -> FreeCat Instr a b -> Instr ('S a)        b

exec1 :: Instr a b -> Vec a Int -> Vec b Int
exec1 And      (x ::: y ::: zs)  =                   x .&. y ::: zs
exec1 Add      (x ::: y ::: zs)  =                     x + y ::: zs
exec1 Eq       (x ::: y ::: zs)  = (if x == y then 1 else 0) ::: zs
exec1 Dup      (      n ::: as)  =                   n ::: n ::: as
exec1 Swap     (x ::: y ::: zs)  =                   y ::: x ::: zs
exec1 (Push x)              zs   =                         x ::: zs
exec1 Pop      (      _ ::: zs)  =                               zs
exec1 (Branch t f) (x ::: zs)
  | x == 0    = exec t zs
  | otherwise = exec f zs

exec :: FreeCat Instr a b -> Vec a Int -> Vec b Int
exec End        zs = zs
exec (i :* is)  zs = exec is (exec1 i zs)

twoAddTwo :: Vec ('S 'Z) Int
twoAddTwo = exec (Add :* End) (2 ::: 2 ::: VNil)

{-

Nth Fibbonacci algorithm

Base case: F(n) = n, when n = 0 or n = 1
Recursive case: F(n) = F(n-1) + F(n-2) for n>1

-}

fib :: FreeCat Instr ('S n) ('S n)
fib =                           -- [n]
  Dup :*                        -- [n, n]
  Branch                        -- [n]
    End -- n == 0              
    ( Dup :*                    -- [n, n]
      Push 1 :*                 -- [n, n, 1]
      Eq :*                     -- [n, n==1]
      Branch                    -- [n]
        ( Dup :*                -- [n, n]
          Push (-1) :*          -- [n, n, -1]
          Add :*                -- [n, n-1]
          fib <:>               -- [n, fib(n-1)]
          Swap :*               -- [fib(n-1), n]
          Push (-2) :*          -- [fib(n-1), n, -2]                
          Add :*                -- [fib(n-1), n-2]
          fib <:>               -- [fib(n-1), fib(n-2)]
          Add :*                -- [fib(n)]
          End
        ) 
        End :* -- n == 1
      End
    )
  :* End

