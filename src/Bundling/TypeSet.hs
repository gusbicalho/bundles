{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Bundling.TypeSet (
  TypeSet,
  Empty,
  Single,
  FromList,
  Elements,
  UnionAll,
  Union,
  Diff,
  Elem,
  UnionRightDiffIsIdentity,
  UnionLeftDiffIsIdentity,
) where

import Data.Kind (Constraint, Type)

newtype TypeSet = TS [Type]

type Empty :: TypeSet
type Empty = 'TS '[]

type Single :: Type -> TypeSet
type Single t = 'TS '[t]

type FromList :: [Type] -> TypeSet
type FromList ts = Union Empty ( 'TS ts)

type Elements :: TypeSet -> [Type]
type family Elements ts where
  Elements ( 'TS types) = types

type UnionAll :: [TypeSet] -> TypeSet
type family UnionAll typeSets where
  UnionAll typeSets = UnionAllInto ( 'TS '[]) typeSets

type UnionAllInto :: TypeSet -> [TypeSet] -> TypeSet
type family UnionAllInto into typeSets where
  UnionAllInto into '[] = into
  UnionAllInto into (set : more) = UnionAllInto (Union into set) more

type Union :: TypeSet -> TypeSet -> TypeSet
type family Union into from where
  Union into ( 'TS '[]) = into
  Union ( 'TS into) ( 'TS (t : more)) = Union ( 'TS (InsertNew t into)) ( 'TS more)

type InsertNew :: Type -> [Type] -> [Type]
type family InsertNew name into where
  InsertNew t '[] = '[t]
  InsertNew t (t : more) = t : more
  InsertNew t (u : more) = u : InsertNew t more

type Diff :: TypeSet -> TypeSet -> TypeSet
type family Diff from toRemove where
  Diff from ( 'TS '[]) = from
  Diff ( 'TS from) ( 'TS (t : moreToRemove)) = Diff ( 'TS (RemoveFrom t from)) ( 'TS moreToRemove)

type RemoveFrom :: Type -> [Type] -> [Type]
type family RemoveFrom t from where
  RemoveFrom t '[] = '[]
  RemoveFrom t (t : more) = more
  RemoveFrom t (u : more) = u : RemoveFrom t more

type UnionRightDiffIsIdentity :: TypeSet -> TypeSet -> Constraint
type UnionRightDiffIsIdentity a b = Union a (Diff a b) ~ a

type UnionLeftDiffIsIdentity :: TypeSet -> TypeSet -> Constraint
type UnionLeftDiffIsIdentity a b = Union (Diff a b) b ~ a

type Elem :: Type -> TypeSet -> Bool
type family Elem t set where
  Elem t ( 'TS '[]) = 'False
  Elem t ( 'TS (t ': _)) = 'True
  Elem t ( 'TS (u ': more)) = Elem t ( 'TS more)
