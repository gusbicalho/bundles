{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling.TypeMap (
  TypeMap,
  TypeMapEntry (..),
  Merge,
  MergeAll,
  InsertNew,
) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

data TypeMapEntry = Symbol :-> Type

type TypeMap = [TypeMapEntry]

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
