{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE DerivingStrategies #-}

{- |
Copyright: (c) 2021 Gustavo Bicalho
SPDX-License-Identifier: MIT
Maintainer: Gustavo Bicalho <gusbicalho@gmail.com>

See README for more info
-}
module Bundling (
  Bundle (..),
  DynamicBundle (..),
  Factory,
  FactorySpec (..),
  runFactory,
  addFromFactory,
  standalone,
  factory,
  dynamicToTyped,
  typedToDynamic,
  Assembler,
  runAssembler,
  assembler,
  assemble,
) where

import Bundling.RepMap (RepMap)
import Bundling.RepMap qualified as RepMap
import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Data.Kind (Constraint, Type)
import Data.Maybe qualified as Maybe
import Debug.Trace qualified as Trace
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:)), Symbol, TypeError)
import HList (HList (HNil, (:::)))

type Maybes :: [Type] -> [Type]
type family Maybes types = maybes | maybes -> types where
  Maybes '[] = '[]
  Maybes (t ': ts) = Maybe t ': Maybes ts

data DynamicBundle meta = DynamicBundle meta RepMap

data Bundle meta types = Bundle meta (HList (Maybes types))

deriving stock instance (Show meta, Show (HList (Maybes types))) => Show (Bundle meta types)

data FactorySpec
  = FactorySpec Symbol Type TypeSet TypeSet

type FactoryBundleMeta :: FactorySpec -> Type
type family FactoryBundleMeta spec where
  FactoryBundleMeta ( 'FactorySpec _ bundleMeta _ _) = bundleMeta

newtype Factory (spec :: FactorySpec) = Factory
  { runFactory :: [DynamicBundle (FactoryBundleMeta spec)] -> [DynamicBundle (FactoryBundleMeta spec)]
  }

newtype Assembler bundleMeta t = Assembler {runAssembler :: [DynamicBundle bundleMeta] -> t}

type AssemblerBundleMeta :: Type -> Type
type family AssemblerBundleMeta spec where
  AssemblerBundleMeta (Assembler bundleMeta _) = bundleMeta
  AssemblerBundleMeta t =
    TypeError
      ( 'Text "AssemblerBundleMeta: Expected an Assembler, but found"
          ':$$: 'ShowType t
      )

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
  ( t ~ HList (Maybes inputs)
  , AllUniqueTypes inputs
  , TestAllEntriesAreEmpty inputs
  , RepMap.FromRepMap t
  )

type ValidInputs inputs = HListOfInputs (HList (Maybes inputs)) inputs

type HListOfOutputs t outputs =
  ( t ~ HList (Maybes outputs)
  , AllUniqueTypes outputs
  , RepMap.ToRepMap t
  )

type ValidOutputs outputs = HListOfOutputs (HList (Maybes outputs)) outputs

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

bundleExports :: Bundle meta types -> HList (Maybes types)
bundleExports (Bundle _ exports) = exports

-- >>> bundleExports <$> dynamicToTyped @'[Int, Char] (DynamicBundle "a" (RepMap.insert (42 :: Int) $ RepMap.empty))
-- Just (Just 42 ::: (Nothing ::: HNil))

typedToDynamic ::
  ValidOutputs types =>
  Bundle meta types ->
  DynamicBundle meta
typedToDynamic (Bundle meta exports) = DynamicBundle meta (RepMap.toRepMap exports)

standalone ::
  forall name types bundleMeta output.
  (HListOfOutputs output types) =>
  bundleMeta ->
  output ->
  Factory ( 'FactorySpec name bundleMeta TS.Empty (TS.FromList types))
standalone meta exportsList = Factory $ const [DynamicBundle meta exports]
 where
  exports = RepMap.toRepMap exportsList

factory ::
  forall name inputs outputs bundleMeta.
  ( ValidInputs inputs
  , ValidOutputs outputs
  ) =>
  ([Bundle bundleMeta inputs] -> [Bundle bundleMeta outputs]) ->
  Factory ( 'FactorySpec name bundleMeta (TS.FromList inputs) (TS.FromList outputs))
factory runFactory = Factory go
 where
  go bundles = typedToDynamic <$> runFactory (Maybe.mapMaybe dynamicToTyped bundles)

assembler ::
  forall inputs output meta.
  ( ValidInputs inputs
  , Show meta
  ) =>
  ([Bundle meta inputs] -> output) ->
  Assembler meta output
assembler doAssemble = Assembler (doAssemble . Maybe.mapMaybe dynamicToTyped . traceMetas)
 where
  traceMetas [] = []
  traceMetas (b@(DynamicBundle meta _) : more) = Trace.traceShow meta `seq` (b : traceMetas more)

class Assemble assemblers bundleMeta where
  type AssembleResults assemblers :: [Type]
  assemble :: HList assemblers -> [DynamicBundle bundleMeta] -> HList (AssembleResults assemblers)

instance Assemble '[] bundleMeta where
  type AssembleResults '[] = '[]
  assemble _ _ = HNil

instance
  ( bundleMeta ~ assemblerBundleMeta
  , Assemble moreAssemblers bundleMeta
  ) =>
  Assemble (Assembler assemblerBundleMeta assemblerResult ': moreAssemblers) bundleMeta
  where
  type
    AssembleResults (Assembler assemblerBundleMeta assemblerResult ': moreAssemblers) =
      assemblerResult ': AssembleResults moreAssemblers
  assemble (asm ::: moreAsms) bundles =
    runAssembler asm bundles ::: assemble moreAsms bundles

addFromFactory ::
  Factory spec ->
  [DynamicBundle (FactoryBundleMeta spec)] ->
  [DynamicBundle (FactoryBundleMeta spec)]
addFromFactory fac bundles = bundles <> runFactory fac bundles
