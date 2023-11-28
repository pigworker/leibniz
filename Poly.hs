module Poly where

data Nat = Z | S Nat
  deriving (Eq, Ord)

data Poly
  =  X
  |  N Nat
  |  P Poly Poly
  |  T Poly Poly
  |  Up Poly
  |  Below Poly
  deriving Show

choose :: Nat -> Nat -> Nat
choose  Z       m  = S Z                              -- [g]
choose (S n)  Z    = Z                                -- [h]
choose (S n) (S m) = choose (S n) m + choose n m      -- [i]

eval :: Poly -> Nat -> Nat
eval X         x     = x                              -- [x]
eval (N n)     x     = n                              -- [n]
eval (P p q) x       = eval p x + eval q x            -- [p]
eval (T p q) x       = eval p x * eval q x            -- [t]
eval (Up p)    x     = eval p (S x)                   -- [u]
eval (Below p) Z     = Z                              -- [z]
eval (Below p) (S n) = eval (Below p) n + eval p n    -- [s]

diff :: Poly -> Poly
diff X         = N (S Z)                              -- [a]
diff (N n)     = N Z                                  -- [b]
diff (P p q)   = P (diff p) (diff q)                  -- [c]
diff (T p q)   = P (T p (diff q)) (T (diff p) (Up q)) -- [d]
diff (Up p)    = Up (diff p)                          -- [e]
diff (Below p) = p                                    -- [f]

{- and now, the examples -}

ex0 :: Nat
ex0 = choose Z Z

ex1 :: Nat
ex1 = choose 2 5

ex2 :: Nat
ex2 = eval X 7

ex3 :: Nat
ex3 = eval (Up (T X X)) 6

ex4 :: Nat
ex4 = eval (Below (Up X)) 4

ex5 :: Nat
ex5 = eval (diff (T X X)) 6


{- bits and pieces -}

instance Show Nat where
  show = show . intify where
    intify Z = 0
    intify (S n) = 1 + intify n

instance Num Nat where
  fromInteger n
    | n > 0 = S (fromInteger (n - 1))
    | otherwise = Z
  Z + y = y
  S x + y = S (x + y)
  Z * y = Z
  S x * y = y + x * y
  x - Z = x
  Z - y = Z
  S x - S y = x - y
  abs n = n
  signum Z = 0
  signum _ = 1

