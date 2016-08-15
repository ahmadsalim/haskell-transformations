{-# LANGUAGE DeriveDataTypeable, DeriveAnyClass, FlexibleInstances, MultiParamTypeClasses #-}
module SimpleRefactorings where

import Nats
import Tip
import Test.LazySmallCheck as LSC
import Data.Generics.Uniplate.Data
import Data.Data
import Data.Set
import Data.Typeable

instance (Ord a, Serial a) => Serial (Set a) where
  series = cons1 fromList

type Name = Nat

eqName :: Name -> Name -> Bool
eqName = eqNat

data Class = Class { class_name :: Name, methods :: Set Method, fields :: Set Field, super :: Name }
  deriving (Show, Eq, Data, Typeable)

instance Serial Class where
  series = cons4 Class

data Expr = ThisExpr {- type -} Name
          | FieldAccesExpr {- type -} Name {- target -} Expr {- field_name -} Name
  deriving (Show, Eq, Ord, Data, Typeable)

instance Serial Expr where
  series = cons1 ThisExpr \/ cons3 FieldAccesExpr

data Method = Method { body :: Expr }
  deriving (Show, Eq, Ord, Data, Typeable)

instance Serial Method where
  series = cons1 Method

data Field = Field { field_name :: Name }
  deriving (Show, Eq, Ord, Data, Typeable)

eqField :: Field -> Field -> Bool
(Field nm) `eqField` (Field nm') = nm `eqName` nm'

instance Serial Field where
  series = cons1 Field

data RenameFieldInput = RFInput { rf_class :: Class, rf_oldField :: Field, rf_newField :: Field }
  deriving (Show, Eq)

instance Serial RenameFieldInput where
  series = cons3 RFInput

hasField :: Set Field -> Field -> Bool
hasField fs f = member f fs

removeField :: Set Field -> Field -> Set Field
removeField fs f = delete f fs

renameField :: Class -> Field -> Field -> Class
renameField cl oldField newField = cl { fields = insert newField (removeField (fields cl) oldField),
                                        methods = transformBi (updateFAReferences oldField newField) (methods cl)                                            }

{-
updateFAReferencesMethods :: Field -> Field -> [Method] -> [Method]
updateFAReferencesMethods oldField newField [] = []
updateFAReferencesMethods oldField newField (Method body:mths) = Method (updateFAReferences oldField newField body) : (updateFAReferencesMethods oldField newField mths)
-}

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

lsc_fieldRenamingClassSame rfinp =
  ((lift $ hasField (fields (rf_class rfinp)) (rf_oldField rfinp)) *&* LSC.neg (lift $ hasField (fields (rf_class rfinp)) (rf_newField rfinp))) *=>*
     (lift $ renameField (rf_class rfinp) (rf_oldField rfinp) (rf_newField rfinp) == (rf_class rfinp))

lsc_fieldRenamingMethodsSame rfinp =
  ((lift $ hasField (fields (rf_class rfinp)) (rf_oldField rfinp)) *&* LSC.neg (lift $ hasField (fields (rf_class rfinp)) (rf_newField rfinp))) *=>*
     (lift $ (methods (renameField (rf_class rfinp) (rf_oldField rfinp) (rf_newField rfinp)) == methods (rf_class rfinp)))
