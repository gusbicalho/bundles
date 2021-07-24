{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module HList (HList (..), (+++), type (+++), type (++), HReverse, hReverse, Reverse) where

import Data.Kind (Constraint, Type)
import GHC.Generics (Generic)

-- definition copied from HList package

type HList :: [Type] -> Type
data family HList ts

data instance HList '[] = HNil
  deriving stock (Generic)

data instance HList (x ': xs) = x ::: HList xs
  deriving stock (Generic)

infixr 5 :::

deriving stock instance Eq (HList '[])

deriving stock instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs))

deriving stock instance Ord (HList '[])

deriving stock instance (Ord x, Ord (HList xs)) => Ord (HList (x ': xs))

deriving stock instance Show (HList '[])
deriving stock instance (Show x, Show (HList xs)) => Show (HList (x ': xs))

-- Concat

type (++) :: forall k. [k] -> [k] -> [k]
type family (++) xs ys where
  (++) '[] ys = ys
  (++) (x ': xs) ys = x ': (xs ++ ys)

type (+++) :: [Type] -> [Type] -> Constraint
class (+++) xs ys where
  (+++) :: HList xs -> HList ys -> HList (xs ++ ys)

instance '[] +++ ys where
  HNil +++ ys = ys

instance (xs +++ ys) => (x ': xs) +++ ys where
  (x ::: xs) +++ ys = x ::: (xs +++ ys)

-- Reverse

type HReverse = GoReverse '[]
type Reverse xs = GoReversed '[] xs
hReverse :: HReverse xs => HList xs -> HList (Reverse xs)
hReverse = goReverse HNil

type GoReversed :: forall k. [k] -> [k] -> [k]
type family GoReversed acc xs where
  GoReversed acc '[] = acc
  GoReversed acc (x ': xs) = GoReversed (x ': acc) xs

type GoReverse :: [Type] -> [Type] -> Constraint
class GoReverse acc xs where
  goReverse :: HList acc -> HList xs -> HList (GoReversed acc xs)

instance GoReverse acc '[] where
  goReverse acc _ = acc

instance (GoReverse (x : acc) xs) => GoReverse acc (x ': xs) where
  goReverse acc (x ::: xs) = goReverse (x ::: acc) xs
