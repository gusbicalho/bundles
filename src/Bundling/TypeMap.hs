{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling.TypeMap (
  KMap,
  KMapEntry (..),
  TypeMap,
  TypeMapEntry,
  Merge,
  MergeAll,
  InsertNew,
  Diff,
  MergeRightDiffIsIdentity,
  MergeLeftDiffIsIdentity,
  Lookup,
) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

data KMapEntry k = Symbol :-> k

type TypeMapEntry = KMapEntry Type

type KMap k = [KMapEntry k]
type TypeMap = KMap Type

type MergeAll :: [TypeMap] -> TypeMap
type family MergeAll typeMaps where
  MergeAll typeMaps = MergeAllInto '[] typeMaps

type MergeAllInto :: TypeMap -> [TypeMap] -> TypeMap
type family MergeAllInto into typeMaps where
  MergeAllInto into '[] = into
  MergeAllInto into (m : more) = MergeAllInto (Merge into m) more

type Merge :: TypeMap -> TypeMap -> TypeMap
type family Merge into from where
  Merge into '[] = into
  Merge into (name ':-> t : more) = Merge (InsertNew name t into) more

type InsertNew :: Symbol -> Type -> TypeMap -> TypeMap
type family InsertNew name t into where
  InsertNew name t '[] = '[name ':-> t]
  InsertNew name t (name ':-> t : more) = name ':-> t : more
  InsertNew name t (name ':-> u : _) =
    TypeError (InsertNewMismatch name t u)
  InsertNew name t (name2 ':-> u : more) = name2 ':-> u : InsertNew name t more

type InsertNewMismatch (name :: Symbol) new found =
  'Text "Member mismatch at key " ':<>: 'Text name
    ':$$: 'Text "Trying to insert type"
    ':$$: 'ShowType new
    ':$$: 'Text "But found existing entry with type"
    ':$$: 'ShowType found

type Diff :: TypeMap -> TypeMap -> TypeMap
type family Diff from toRemove where
  Diff from '[] = from
  Diff from (name ':-> t : moreToRemove) = Diff (RemoveFrom name t from) moreToRemove

type RemoveFrom :: Symbol -> Type -> TypeMap -> TypeMap
type family RemoveFrom name t from where
  RemoveFrom name t '[] = '[]
  RemoveFrom name t (name ':-> t : more) = more
  RemoveFrom name t (name ':-> u : _) =
    TypeError (RemoveFromMismatch name t u)
  RemoveFrom name t (name2 ':-> u : more) = name2 ':-> u : RemoveFrom name t more

type RemoveFromMismatch (name :: Symbol) expected found =
  'Text "Member mismatch at key " ':<>: 'Text name
    ':$$: 'Text "Trying to remove type"
    ':$$: 'ShowType expected
    ':$$: 'Text "But found existing entry with type"
    ':$$: 'ShowType found

type MergeRightDiffIsIdentity :: TypeMap -> TypeMap -> Constraint
type MergeRightDiffIsIdentity a b = Merge a (Diff a b) ~ a

type MergeLeftDiffIsIdentity :: TypeMap -> TypeMap -> Constraint
type MergeLeftDiffIsIdentity a b = Merge (Diff a b) b ~ a

type Lookup :: Symbol -> TypeMap -> Type
type family Lookup name m where
  Lookup name m = LookupGo m name m

type LookupGo :: TypeMap -> Symbol -> TypeMap -> Type
type family LookupGo original name m where
  LookupGo original name '[] = TypeError (LookupNotFound original name)
  LookupGo original name (name ':-> t : more) = t
  LookupGo original name (_ : more) = LookupGo original name more

type LookupNotFound :: TypeMap -> Symbol -> ErrorMessage
type family LookupNotFound map name where
