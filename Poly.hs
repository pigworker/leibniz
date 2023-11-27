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

eval :: Poly -> Nat -> Nat
eval X         x     = x                           -- [x]
eval (N n)     x     = n                           -- [n]
eval (P p q) x       = eval p x + eval q x         -- [p]
eval (T p q) x       = eval p x * eval q x         -- [t]
eval (Up p)    x     = eval p (S x)                -- [u]
eval (Below p) Z     = Z                           -- [z]
eval (Below p) (S n) = eval (Below p) n + eval p n -- [s]

diff :: Poly -> Poly
diff X         = N (S Z)                         -- [a]
diff (N n)     = Z                               -- [b]
diff (P p q)   = P (diff p) (diff q)             -- [c]
diff (T p q)   = P (T (diff p) q) (T p (Up q))   -- [d]
diff (Up p)    = Up (diff p)                     -- [e]
diff (Below p) = p                               -- [f]

ex1 :: Poly
ex1 = diff (T X (Up X))

{-
ex0 :: Nat
ex0 = eval (Below (P X (N 1))) 5
-}

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

