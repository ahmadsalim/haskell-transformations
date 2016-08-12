module Refactoring where

import Nats
import Tip
import Test.LazySmallCheck

type Name = Nat

data Class = Class { class_name :: Name, methods :: [Method], fields :: [Field], super :: Name }
  deriving (Show, Eq)

instance Serial Class where
  series = cons4 Class

data Method = Method { body :: Expr }
  deriving (Show, Eq)

instance Serial Method where
  series = cons1 Method

data Field = Field { field_name :: Name }
  deriving (Show, Eq)

eqField :: Field -> Field -> Bool
(Field nm) `eqField` (Field nm') = nm `eqNat` nm'

instance Serial Field where
  series = cons1 Field

data Expr = ThisExpr { type_ :: Name }
          | FieldAccesExpr { type_ :: Name, target :: Expr,  field :: Name }
  deriving (Show, Eq)

instance Serial Expr where
  series = cons1 ThisExpr \/ cons3 FieldAccesExpr

hasField :: [Field] -> Field -> Bool
hasField [] _ = False
hasField (f:fs) ff = if f `eqField` ff then True else hasField fs ff

removeField :: [Field] -> Field -> [Field]
removeField [] _ = []
removeField (f : fs) ff | f `eqField` ff = removeField fs ff
                        | otherwise      = f : removeField fs ff

renameField :: Class -> Field -> Field -> Class
renameField cl oldField newField = cl { fields = newField : (removeField (fields cl) oldField) }
