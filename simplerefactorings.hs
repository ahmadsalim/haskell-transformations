module SimpleRefactorings where

import Nats
import Tip
import Test.LazySmallCheck as LSC

type Name = Nat

eqName :: Name -> Name -> Bool
eqName = eqNat

data Class = Class { class_name :: Name, methods :: [Method], fields :: [Field], super :: Name }
  deriving (Show, Eq)

instance Serial Class where
  series = cons4 Class

data Expr = ThisExpr {- type -} Name
          | FieldAccesExpr {- type -} Name {- target -} Expr {- field_name -} Name
  deriving (Show, Eq)

instance Serial Expr where
  series = cons1 ThisExpr \/ cons3 FieldAccesExpr

data Method = Method { body :: Expr }
  deriving (Show, Eq)

instance Serial Method where
  series = cons1 Method

data Field = Field { field_name :: Name }
  deriving (Show, Eq)

eqField :: Field -> Field -> Bool
(Field nm) `eqField` (Field nm') = nm `eqName` nm'

instance Serial Field where
  series = cons1 Field


data RenameFieldInput = RFInput { rf_class :: Class, rf_oldField :: Field, rf_newField :: Field }
  deriving (Show, Eq)

instance Serial RenameFieldInput where
  series = cons3 RFInput

hasField :: [Field] -> Field -> Bool
hasField [] _ = False
hasField (f:fs) ff = if f `eqField` ff then True else hasField fs ff

removeField :: [Field] -> Field -> [Field]
removeField [] _ = []
removeField (f : fs) ff | f `eqField` ff = removeField fs ff
                        | otherwise      = f : removeField fs ff

renameField :: Class -> Field -> Field -> Class
renameField cl oldField newField = cl { fields = newField : (removeField (fields cl) oldField), methods = updateFAReferencesMethods oldField newField (methods cl) }

updateFAReferencesMethods :: Field -> Field -> [Method] -> [Method]
updateFAReferencesMethods oldField newField [] = []
updateFAReferencesMethods oldField newField (Method body:mths) = Method (updateFAReferences oldField newField body) : (updateFAReferencesMethods oldField newField mths)

updateFAReferences :: Field -> Field -> Expr -> Expr
updateFAReferences oldField newField (ThisExpr typ) = ThisExpr typ
updateFAReferences oldField newField (FieldAccesExpr typ tar fnm) | fnm `eqName` (field_name oldField) = FieldAccesExpr typ tar (field_name newField)
                                                                  | otherwise = FieldAccesExpr typ tar fnm


prop_fieldRenamingClassSame rfinp =
  (hasField (fields (rf_class rfinp)) (rf_oldField rfinp) && not (hasField (fields (rf_class rfinp)) (rf_newField rfinp))) Tip.==>
     (renameField (rf_class rfinp) (rf_oldField rfinp) (rf_newField rfinp) Tip.=== (rf_class rfinp))

prop_fieldRenamingMethodsSame rfinp =
  (hasField (fields (rf_class rfinp)) (rf_oldField rfinp) && not (hasField (fields (rf_class rfinp)) (rf_newField rfinp))) Tip.==>
     (methods (renameField (rf_class rfinp) (rf_oldField rfinp) (rf_newField rfinp)) Tip.=== methods (rf_class rfinp))

