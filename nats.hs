{-# LANGUAGE DeriveDataTypeable, DeriveAnyClass #-}
module Nats where

import Data.Data
import Data.Typeable
import Test.LazySmallCheck

data Nat = Z | S Nat
  deriving (Show, Eq, Ord, Data, Typeable)

instance Serial Nat where
  series = cons0 Z \/ cons1 S

eqNat :: Nat -> Nat -> Bool
Z `eqNat` Z = True
(S x) `eqNat` (S y) = x `eqNat` y
_ `eqNat` _ = False
