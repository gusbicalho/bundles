{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling.Bundle (
  Bundle (..),
  DynamicBundle (..),
  Maybes,
  HListOfInputs,
  ValidInputs,
  HListOfOutputs,
  ValidOutputs,
  dynamicToTyped,
  typedToDynamic,
  bundleExports,
) where

import Bundling.RepMap (RepMap)
import Bundling.RepMap qualified as RepMap
import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Data.Kind (Constraint, Type)
import HList (HList (HNil, (:::)))

type Maybes :: [Type] -> [Type]
type family Maybes types = maybes | maybes -> types where
  Maybes '[] = '[]
  Maybes (t ': ts) = Maybe t ': Maybes ts

data DynamicBundle meta = DynamicBundle meta RepMap

type Bundle :: Type -> TypeSet -> Type
data Bundle meta types where
  Bundle ::
    meta ->
    HList (Maybes (TS.Elements types)) ->
    Bundle meta types

deriving stock instance (Show meta, Show (HList (Maybes (TS.Elements types)))) => Show (Bundle meta types)

type AllUniqueTypes :: [Type] -> Constraint
type AllUniqueTypes types = types ~ TS.Elements (TS.FromList types)

class TestAllEntriesAreEmpty types where
  allEntriesAreEmpty :: HList (Maybes types) -> Bool

instance TestAllEntriesAreEmpty '[] where
  allEntriesAreEmpty HNil = True

instance TestAllEntriesAreEmpty types => TestAllEntriesAreEmpty (t ': types) where
  allEntriesAreEmpty (a ::: more) = case a of
    Just _ -> False
    Nothing -> allEntriesAreEmpty more

type HListOfInputs t inputs =
  ( t ~ HList (Maybes (TS.Elements inputs))
  , AllUniqueTypes (TS.Elements inputs)
  , TestAllEntriesAreEmpty (TS.Elements inputs)
  , RepMap.FromRepMap t
  )

type ValidInputs inputs = HListOfInputs (HList (Maybes (TS.Elements inputs))) inputs

type HListOfOutputs t outputs =
  ( t ~ HList (Maybes (TS.Elements outputs))
  , AllUniqueTypes (TS.Elements outputs)
  , RepMap.ToRepMap t
  )

type ValidOutputs outputs = HListOfOutputs (HList (Maybes (TS.Elements outputs))) outputs

dynamicToTyped ::
  forall types meta.
  ValidInputs types =>
  DynamicBundle meta ->
  Maybe (Bundle meta types)
dynamicToTyped (DynamicBundle bundleMeta repMap) =
  case RepMap.fromRepMap repMap of
    typedExports
      | allEntriesAreEmpty typedExports -> Nothing
      | otherwise -> Just (Bundle bundleMeta typedExports)

bundleExports :: Bundle meta types -> HList (Maybes (TS.Elements types))
bundleExports (Bundle _ exports) = exports

typedToDynamic ::
  ValidOutputs types =>
  Bundle meta types ->
  DynamicBundle meta
typedToDynamic (Bundle meta exports) = DynamicBundle meta (RepMap.toRepMap exports)
