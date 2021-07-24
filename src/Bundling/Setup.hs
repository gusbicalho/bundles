{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Bundling.Setup (
  Setup (..),
  assembleSetup,
  runSetup,
  BuildSetup,
  buildSetup,
) where

import Bundling.Assemble (Assemble)
import Bundling.Assemble qualified as Assemble
import Bundling.Bundle (DynamicBundle)
import Bundling.Factory (Factories, Factory, FactorySpec (..))
import Bundling.Factory qualified as Factory
import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (ErrorMessage (Text, (:$$:)), TypeError)
import HList (HList (HNil, (:::)), Reverse, (+++), type (++), type (+++))
import HList qualified

type Setup :: Type -> [FactorySpec] -> Type
data Setup bundleMeta factorySpecs where
  Empty :: Setup bundleMeta '[]
  (:>>) ::
    ( Factory.ValidFactory spec
    , Factory.FactoryBundleMeta spec ~ bundleMeta
    ) =>
    Factory spec ->
    Setup bundleMeta specs ->
    Setup bundleMeta (spec ': specs)

infixr 5 :>>

assembleSetup ::
  forall bundleMeta specs assemblers.
  ( Assemble assemblers bundleMeta
  , BuildSetup bundleMeta specs
  ) =>
  assemblers ->
  HList (Factories specs) ->
  Assemble.AssembleResults assemblers
assembleSetup assemblers = Assemble.assemble assemblers . runSetup [] . buildSetup @bundleMeta

runSetup :: [DynamicBundle bundleMeta] -> Setup bundleMeta specs -> [DynamicBundle bundleMeta]
runSetup bundles setup = case setup of
  Empty -> bundles
  factory :>> more -> runSetup (Factory.addFromFactory factory bundles) more

-- BuildSetup takes an HList of Factories and builds a setups
-- I am not proud of this implementation

type BuildSetup :: Type -> [FactorySpec] -> Constraint
class
  ( Factory.ValidFactories specs
  , Factory.FactoriesHaveSameMeta bundleMeta specs
  ) =>
  BuildSetup bundleMeta specs
  where
  type BuildSetupResult specs :: [FactorySpec]
  buildSetup ::
    HList (Factories specs) ->
    Setup bundleMeta (BuildSetupResult specs)

instance BuildSetup bundleMeta '[] where
  type BuildSetupResult '[] = '[]
  buildSetup HNil = Empty

instance
  ( Factory.ValidFactories (spec_ : moreSpecs_)
  , Factory.FactoriesHaveSameMeta bundleMeta (spec_ ': moreSpecs_)
  , PullForwardFirstFactoryWhichDoesNotNeedFutureOutput
      bundleMeta
      (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
      spec_
      moreSpecs_
  , (readySpec : nextSpecs)
      ~ PullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
          (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
          spec_
          moreSpecs_
  , BuildSetup bundleMeta nextSpecs
  ) =>
  BuildSetup bundleMeta (spec_ ': moreSpecs_)
  where
  type
    BuildSetupResult (spec_ ': moreSpecs_) =
      GoBuildSetupResult
        ( PullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
            (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
            spec_
            moreSpecs_
        )
  buildSetup factories =
    case pullForwardFirstFactoryWhichDoesNotNeedFutureOutput
      (Proxy @(Factory.AllFactoryOutputs (spec_ : moreSpecs_)))
      (Proxy @bundleMeta)
      factories of
      nextFactory ::: moreFactories -> nextFactory :>> buildSetup moreFactories

type family GoBuildSetupResult specs where
  GoBuildSetupResult '[] = '[]
  GoBuildSetupResult (spec : moreSpecs) = spec : BuildSetupResult moreSpecs

type PullForwardFirstFactoryWhichDoesNotNeedFutureOutput :: Type -> TypeSet -> FactorySpec -> [FactorySpec] -> Constraint
class
  ( Factory.ValidFactories (spec : moreSpecs)
  , Factory.FactoriesHaveSameMeta bundleMeta (spec : moreSpecs)
  , Factory.ValidFactories
      ( PullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
          futureOutputs
          spec
          moreSpecs
      )
  , Factory.FactoriesHaveSameMeta
      bundleMeta
      ( PullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
          futureOutputs
          spec
          moreSpecs
      )
  ) =>
  PullForwardFirstFactoryWhichDoesNotNeedFutureOutput bundleMeta futureOutputs spec moreSpecs
  where
  type PullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult futureOutputs spec moreSpecs :: [FactorySpec]
  pullForwardFirstFactoryWhichDoesNotNeedFutureOutput ::
    Proxy futureOutputs ->
    Proxy bundleMeta ->
    HList (Factories (spec ': moreSpecs)) ->
    HList (Factories (PullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult futureOutputs spec moreSpecs))

instance
  ( Factory.ValidFactories (spec : moreSpecs)
  , Factory.FactoriesHaveSameMeta bundleMeta (spec : moreSpecs)
  , Factory.ValidFactories
      ( PullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
          futureOutputs
          spec
          moreSpecs
      )
  , Factory.FactoriesHaveSameMeta
      bundleMeta
      ( PullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
          futureOutputs
          spec
          moreSpecs
      )
  , GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutput
      bundleMeta
      (Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs)
      futureOutputs
      '[]
      spec
      moreSpecs
  ) =>
  PullForwardFirstFactoryWhichDoesNotNeedFutureOutput bundleMeta futureOutputs spec moreSpecs
  where
  type
    PullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult futureOutputs spec moreSpecs =
      GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
        (Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs)
        futureOutputs
        '[]
        spec
        moreSpecs
  pullForwardFirstFactoryWhichDoesNotNeedFutureOutput _ _ (spec ::: moreSpecs) =
    goPullForwardFirstFactoryWhichDoesNotNeedFutureOutput
      (Proxy @bundleMeta)
      (Proxy @(Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs))
      (Proxy @futureOutputs)
      HNil
      (spec ::: moreSpecs)

type GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutput ::
  Type -> Bool -> TypeSet -> [FactorySpec] -> FactorySpec -> [FactorySpec] -> Constraint
class
  ( Factories (Reverse previousSpecs ++ moreSpecs)
      ~ (Factories (Reverse previousSpecs) ++ Factories moreSpecs)
  , Factories (Reverse previousSpecs) ~ Reverse (Factories previousSpecs)
  ) =>
  GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutput
    bundleMeta
    specDoesNotNeedFutureOutput
    futureOutputs
    previousSpecs
    spec
    moreSpecs
  where
  type
    GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
      specDoesNotNeedFutureOutput
      futureOutputs
      previousSpecs
      spec
      moreSpecs ::
      [FactorySpec]
  goPullForwardFirstFactoryWhichDoesNotNeedFutureOutput ::
    Proxy bundleMeta ->
    Proxy specDoesNotNeedFutureOutput ->
    Proxy futureOutputs ->
    HList (Factories previousSpecs) ->
    HList (Factories (spec ': moreSpecs)) ->
    HList
      ( Factories
          (GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult specDoesNotNeedFutureOutput futureOutputs previousSpecs spec moreSpecs)
      )

instance
  ( Factories (Reverse previousSpecs ++ moreSpecs)
      ~ (Factories (Reverse previousSpecs) ++ Factories moreSpecs)
  , Factories (Reverse previousSpecs) ~ Reverse (Factories previousSpecs)
  , Reverse (Factories previousSpecs) +++ Factories moreSpecs
  , HList.HReverse (Factories previousSpecs)
  ) =>
  GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutput
    bundleMeta
    'True
    futureOutputs
    previousSpecs
    spec
    moreSpecs
  where
  type
    GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
      'True
      futureOutputs
      previousSpecs
      spec
      moreSpecs =
      spec : (Reverse previousSpecs ++ moreSpecs)
  goPullForwardFirstFactoryWhichDoesNotNeedFutureOutput _ _ _ previous (factory ::: more) =
    factory ::: (HList.hReverse previous +++ more)

instance
  ( Factories (Reverse previousSpecs ++ '[])
      ~ (Factories (Reverse previousSpecs) ++ Factories '[])
  , Factories (Reverse previousSpecs) ~ Reverse (Factories previousSpecs)
  ) =>
  GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutput
    bundleMeta
    'False
    futureOutputs
    previousSpecs
    spec
    '[]
  where
  type
    GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
      'False
      futureOutputs
      previousSpecs
      spec
      '[] =
      TypeError
        ( ListAllFactoryNamesForError
            ( 'Text "Cannot make progress due to cycle between factories")
            (Reverse (spec : previousSpecs))
        )
  goPullForwardFirstFactoryWhichDoesNotNeedFutureOutput _ _ _ _ _ =
    error "Circular dependencies"

instance
  ( Factories (Reverse previousSpecs ++ (nextSpec : moreSpecs))
      ~ (Factories (Reverse previousSpecs) ++ Factories (nextSpec : moreSpecs))
  , Factories (Reverse previousSpecs) ~ Reverse (Factories previousSpecs)
  , GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutput
      bundleMeta
      ( TS.HasNoIntersectionWith
          (Factory.FactoryInputs nextSpec)
          futureOutputs
      )
      futureOutputs
      (spec : previousSpecs)
      nextSpec
      moreSpecs
  ) =>
  GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutput
    bundleMeta
    'False
    futureOutputs
    previousSpecs
    spec
    (nextSpec : moreSpecs)
  where
  type
    GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
      'False
      futureOutputs
      previousSpecs
      spec
      (nextSpec : moreSpecs) =
      GoPullForwardFirstFactoryWhichDoesNotNeedFutureOutputResult
        (Factory.FactoryInputs nextSpec `TS.HasNoIntersectionWith` futureOutputs)
        futureOutputs
        (spec : previousSpecs)
        nextSpec
        moreSpecs
  goPullForwardFirstFactoryWhichDoesNotNeedFutureOutput _ _ _ previous (factory ::: more) =
    goPullForwardFirstFactoryWhichDoesNotNeedFutureOutput
      (Proxy @bundleMeta)
      (Proxy @(Factory.FactoryInputs nextSpec `TS.HasNoIntersectionWith` futureOutputs))
      (Proxy @futureOutputs)
      (factory ::: previous)
      more

type ListAllFactoryNamesForError :: ErrorMessage -> [FactorySpec] -> ErrorMessage
type family ListAllFactoryNamesForError prefix specs where
  ListAllFactoryNamesForError prefix '[] = prefix
  ListAllFactoryNamesForError prefix (spec ': specs) =
    ListAllFactoryNamesForError (prefix ':$$: 'Text (Factory.FactoryName spec)) specs

-- TODO

type If :: forall k. Bool -> k -> k -> k
type family If c t e where
  If 'True t _ = t
  If 'False _ e = e
