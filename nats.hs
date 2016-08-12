module Nats where

import Test.LazySmallCheck

data Nat = Z | S Nat
  deriving (Show, Eq)

instance Serial Nat where
  series = cons0 Z \/ cons1 S

eqNat :: Nat -> Nat -> Bool
Z `eqNat` Z = True
(S x) `eqNat` (S y) = x `eqNat` y
_ `eqNat` _ = False
