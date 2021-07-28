{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling.Setup.PopNextReadyFactory (
  PopNextReadyFactory,
  NextReadyFactoryPoppedSpecs,
  popNextReadyFactory,
) where

-- Given an HList of Factories, pops the first one that does not require any of
-- the futureOutputs as its inputs.

import Bundling.Factory (Factories, FactorySpec)
import Bundling.Factory qualified as Factory
import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (Text, (:$$:)), TypeError)
import HList (HList (HNil, (:::)), Reverse, type (++), type (+++) ((+++)))
import HList qualified

type PopNextReadyFactory :: TypeSet -> FactorySpec -> [FactorySpec] -> (Type -> Type) -> Constraint
type PopNextReadyFactory futureOutputs spec moreSpecs =
  GoPopNextReadyFactory
    (Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs)
    futureOutputs
    '[]
    spec
    moreSpecs

type NextReadyFactoryPoppedSpecs :: TypeSet -> FactorySpec -> [FactorySpec] -> ([FactorySpec], [FactorySpec])
type NextReadyFactoryPoppedSpecs futureOutputs spec moreSpecs =
  GoNextReadyFactoryPoppedSpecs
    (Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs)
    futureOutputs
    '[]
    spec
    moreSpecs

popNextReadyFactory ::
  forall futureOutputs spec moreSpecs m.
  PopNextReadyFactory futureOutputs spec moreSpecs m =>
  HList (Factories m (spec ': moreSpecs)) ->
  NextReadyFactoriesPopped m (NextReadyFactoryPoppedSpecs futureOutputs spec moreSpecs)
{-# INLINE popNextReadyFactory #-}
popNextReadyFactory (spec ::: moreSpecs) =
  goPopNextReadyFactory
    @(Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs)
    @futureOutputs
    HNil
    (spec ::: moreSpecs)

type GoPopNextReadyFactory ::
  Bool -> TypeSet -> [FactorySpec] -> FactorySpec -> [FactorySpec] -> (Type -> Type) -> Constraint
class
  GoPopNextReadyFactory
    specDoesNotNeedFutureOutput
    futureOutputs
    previousSpecs
    spec
    moreSpecs
    m
  where
  type
    GoNextReadyFactoryPoppedSpecs
      specDoesNotNeedFutureOutput
      futureOutputs
      previousSpecs
      spec
      moreSpecs ::
      ([FactorySpec], [FactorySpec])
  goPopNextReadyFactory ::
    HList (Factories m previousSpecs) ->
    HList (Factories m (spec ': moreSpecs)) ->
    NextReadyFactoriesPopped
      m
      (GoNextReadyFactoryPoppedSpecs specDoesNotNeedFutureOutput futureOutputs previousSpecs spec moreSpecs)

type NextReadyFactoriesPopped :: (Type -> Type) -> ([FactorySpec], [FactorySpec]) -> Type
type family NextReadyFactoriesPopped m poppedSpecs where
  NextReadyFactoriesPopped m '(readySpecs, moreSpecs) =
    (HList (Factories m readySpecs), HList (Factories m moreSpecs))

instance
  ( Factories m (Reverse previousSpecs ++ moreSpecs)
      ~ (Factories m (Reverse previousSpecs) ++ Factories m moreSpecs)
  , Factories m (Reverse previousSpecs) ~ Reverse (Factories m previousSpecs)
  , Reverse (Factories m previousSpecs) +++ Factories m moreSpecs
  , HList.HReverse (Factories m previousSpecs)
  ) =>
  GoPopNextReadyFactory
    'True
    futureOutputs
    previousSpecs
    spec
    moreSpecs
    m
  where
  type
    GoNextReadyFactoryPoppedSpecs
      'True
      futureOutputs
      previousSpecs
      spec
      moreSpecs =
      '( '[spec], Reverse previousSpecs ++ moreSpecs)
  {-# INLINE goPopNextReadyFactory #-}
  goPopNextReadyFactory previous (factory ::: more) =
    (factory ::: HNil, HList.hReverse previous +++ more)

instance
  GoPopNextReadyFactory
    'False
    futureOutputs
    previousSpecs
    spec
    '[]
    m
  where
  type
    GoNextReadyFactoryPoppedSpecs
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
  {-# INLINE goPopNextReadyFactory #-}
  goPopNextReadyFactory _ _ =
    error "Circular dependencies"

instance
  ( Factories m (Reverse previousSpecs ++ (nextSpec : moreSpecs))
      ~ (Factories m (Reverse previousSpecs) ++ Factories m (nextSpec : moreSpecs))
  , Factories m (Reverse previousSpecs) ~ Reverse (Factories m previousSpecs)
  , GoPopNextReadyFactory
      ( TS.HasNoIntersectionWith
          (Factory.FactoryInputs nextSpec)
          futureOutputs
      )
      futureOutputs
      (spec : previousSpecs)
      nextSpec
      moreSpecs
      m
  ) =>
  GoPopNextReadyFactory
    'False
    futureOutputs
    previousSpecs
    spec
    (nextSpec : moreSpecs)
    m
  where
  type
    GoNextReadyFactoryPoppedSpecs
      'False
      futureOutputs
      previousSpecs
      spec
      (nextSpec : moreSpecs) =
      GoNextReadyFactoryPoppedSpecs
        (Factory.FactoryInputs nextSpec `TS.HasNoIntersectionWith` futureOutputs)
        futureOutputs
        (spec : previousSpecs)
        nextSpec
        moreSpecs
  {-# INLINE goPopNextReadyFactory #-}
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
