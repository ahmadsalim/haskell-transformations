{-# LANGUAGE ScopedTypeVariables #-}
module HigherOrderTesting where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Poly
import Data.List
import Data.List.Ordered

lteBy :: (a -> a -> Ordering) -> (a -> a -> Bool)
lteBy f x y = f x y /= GT

isReflexive, isTransitive, isAntisymmetric, isTotal :: (a -> a -> Ordering) -> [a] -> Bool

isReflexive f xs = all (\x -> f x x /= GT) xs

isTransitive f xs = all (\(x, y, z) -> not (lteBy f x y && lteBy f y z) || lteBy f x z) [ (x, y, z) | x <- xs, y <- xs, z <- xs ]

isAntisymmetric f xs = all (\(x, y) -> not (lteBy f x y && lteBy f y x) || f x y == EQ) [ (x, y) | x <- xs, y <- xs ]

isTotal f xs = all (\(x, y) -> lteBy f x y || lteBy f y x) [(x, y) | x <- xs, y <- xs ]

isTotalOrder :: (a -> a -> Ordering) -> [a] -> Bool
isTotalOrder f xs = isReflexive f xs && isTransitive f xs && isAntisymmetric f xs && isTotal f xs

prop_SortBy (Fun _ f, (xs :: [A])) = isTotalOrder (curry f) xs ==>
  case sortBy (curry f) $ xs of
   [] -> True
   (y:ys) -> isSortedBy (lteBy (curry f)) (ys ++ [y])

followsLeftIdentity, followsRightIdentity, followsAssociativity :: Eq a => a -> (a -> a -> a) -> [a] -> Bool

followsLeftIdentity meps mplus xs = all (\x -> mplus meps x == x) xs

followsRightIdentity meps mplus xs = all (\x -> mplus x meps == x) xs

followsAssociativity meps mplus xs = all (\(x, y, z) -> mplus x (mplus y z) == mplus (mplus x y) z) [ (x, y, z) | x <- xs, y <- xs, z <- xs ]

isMonoidal :: Eq a => a -> (a -> a -> a) -> [a] -> Bool
isMonoidal meps mplus xs = followsLeftIdentity meps mplus xs && followsRightIdentity meps mplus xs && followsAssociativity meps mplus xs

prop_Monoidic (meps, Fun _ mplus, xs :: [A]) = isMonoidal meps (curry mplus) xs ==> all (\x -> curry mplus x x == x) xs

