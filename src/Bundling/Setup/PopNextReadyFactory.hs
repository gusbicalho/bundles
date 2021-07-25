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

module Bundling.Setup.PopNextReadyFactory (
  PopNextReadyFactory,
  NextReadyFactoryPopped,
  popNextReadyFactory,
  NonEmptyFactories,
) where

-- Given an HList of Factories, pops the first one that does not require any of
-- the futureOutputs as its inputs.

import Bundling.Factory (Factories, Factory, FactorySpec)
import Bundling.Factory qualified as Factory
import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.TypeLits (ErrorMessage (Text, (:$$:)), TypeError)
import HList (HList (HNil, (:::)), Reverse, type (++), type (+++) ((+++)))
import HList qualified

type PopNextReadyFactory :: TypeSet -> FactorySpec -> [FactorySpec] -> Constraint
type PopNextReadyFactory futureOutputs spec moreSpecs =
  ( GoPopNextReadyFactory
      (Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs)
      futureOutputs
      '[]
      spec
      moreSpecs
  )

type NextReadyFactoryPopped futureOutputs spec moreSpecs =
  GoNextReadyFactoryPopped
    (Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs)
    futureOutputs
    '[]
    spec
    moreSpecs

popNextReadyFactory ::
  forall futureOutputs spec moreSpecs.
  PopNextReadyFactory futureOutputs spec moreSpecs =>
  HList (Factories (spec ': moreSpecs)) ->
  NonEmptyFactories (NextReadyFactoryPopped futureOutputs spec moreSpecs)
popNextReadyFactory (spec ::: moreSpecs) =
  goPopNextReadyFactory
    @(Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs)
    @futureOutputs
    HNil
    (spec ::: moreSpecs)

type GoPopNextReadyFactory ::
  Bool -> TypeSet -> [FactorySpec] -> FactorySpec -> [FactorySpec] -> Constraint
class
  ( Factories (Reverse previousSpecs ++ moreSpecs)
      ~ (Factories (Reverse previousSpecs) ++ Factories moreSpecs)
  , Factories (Reverse previousSpecs) ~ Reverse (Factories previousSpecs)
  ) =>
  GoPopNextReadyFactory
    specDoesNotNeedFutureOutput
    futureOutputs
    previousSpecs
    spec
    moreSpecs
  where
  type
    GoNextReadyFactoryPopped
      specDoesNotNeedFutureOutput
      futureOutputs
      previousSpecs
      spec
      moreSpecs ::
      NonEmpty FactorySpec
  goPopNextReadyFactory ::
    HList (Factories previousSpecs) ->
    HList (Factories (spec ': moreSpecs)) ->
    NonEmptyFactories
      (GoNextReadyFactoryPopped specDoesNotNeedFutureOutput futureOutputs previousSpecs spec moreSpecs)

type NonEmptyFactories :: NonEmpty FactorySpec -> Type
type family NonEmptyFactories nonEmptySpecs where
  NonEmptyFactories (spec ':| specs) = (Factory spec, HList (Factories specs))

instance
  ( Factories (Reverse previousSpecs ++ moreSpecs)
      ~ (Factories (Reverse previousSpecs) ++ Factories moreSpecs)
  , Factories (Reverse previousSpecs) ~ Reverse (Factories previousSpecs)
  , Reverse (Factories previousSpecs) +++ Factories moreSpecs
  , HList.HReverse (Factories previousSpecs)
  ) =>
  GoPopNextReadyFactory
    'True
    futureOutputs
    previousSpecs
    spec
    moreSpecs
  where
  type
    GoNextReadyFactoryPopped
      'True
      futureOutputs
      previousSpecs
      spec
      moreSpecs =
      (spec ':| Reverse previousSpecs ++ moreSpecs)
  goPopNextReadyFactory previous (factory ::: more) =
    (factory, HList.hReverse previous +++ more)

instance
  ( Factories (Reverse previousSpecs ++ '[])
      ~ (Factories (Reverse previousSpecs) ++ Factories '[])
  , Factories (Reverse previousSpecs) ~ Reverse (Factories previousSpecs)
  ) =>
  GoPopNextReadyFactory
    'False
    futureOutputs
    previousSpecs
    spec
    '[]
  where
  type
    GoNextReadyFactoryPopped
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
  goPopNextReadyFactory _ _ =
    error "Circular dependencies"

instance
  ( Factories (Reverse previousSpecs ++ (nextSpec : moreSpecs))
      ~ (Factories (Reverse previousSpecs) ++ Factories (nextSpec : moreSpecs))
  , Factories (Reverse previousSpecs) ~ Reverse (Factories previousSpecs)
  , GoPopNextReadyFactory
      ( TS.HasNoIntersectionWith
          (Factory.FactoryInputs nextSpec)
          futureOutputs
      )
      futureOutputs
      (spec : previousSpecs)
      nextSpec
      moreSpecs
  ) =>
  GoPopNextReadyFactory
    'False
    futureOutputs
    previousSpecs
    spec
    (nextSpec : moreSpecs)
  where
  type
    GoNextReadyFactoryPopped
      'False
      futureOutputs
      previousSpecs
      spec
      (nextSpec : moreSpecs) =
      GoNextReadyFactoryPopped
        (Factory.FactoryInputs nextSpec `TS.HasNoIntersectionWith` futureOutputs)
        futureOutputs
        (spec : previousSpecs)
        nextSpec
        moreSpecs
  goPopNextReadyFactory previous (factory ::: more) =
    goPopNextReadyFactory
      @(Factory.FactoryInputs nextSpec `TS.HasNoIntersectionWith` futureOutputs)
      @futureOutputs
      (factory ::: previous)
      more

type ListAllFactoryNamesForError :: ErrorMessage -> [FactorySpec] -> ErrorMessage
type family ListAllFactoryNamesForError prefix specs where
  ListAllFactoryNamesForError prefix '[] = prefix
  ListAllFactoryNamesForError prefix (spec ': specs) =
    ListAllFactoryNamesForError (prefix ':$$: 'Text (Factory.FactoryName spec)) specs

-- Utils

type NonEmptyToList :: NonEmpty k -> [k]
type family NonEmptyToList nonEmpty where
  NonEmptyToList (t ':| ts) = t : ts
