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

-- Given an HList of Factories, pops the first one that does not require any of
-- the futureOutputs as its inputs.
import Bundling.Factory (Factories, Factory, FactorySpec)
import Bundling.Factory qualified as Factory
import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (Text, (:$$:)), TypeError)
import HList (HList (HNil, (:::)), Reverse, type (++))
import HList qualified

type PopNextReadyFactory :: TypeSet -> FactorySpec -> [FactorySpec] -> (Type -> Type) -> Constraint
type PopNextReadyFactory futureOutputs spec moreSpecs m =
  ( GoPopNextReadyFactory
      (Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs)
      futureOutputs
      '[]
      '[]
      spec
      moreSpecs
      m
  , NextReadyFactoryPoppedSpecs futureOutputs spec moreSpecs
      ~ UncheckedNextReadyFactoryPoppedSpecs futureOutputs spec moreSpecs
  )

type NextReadyFactoryPoppedSpecs :: TypeSet -> FactorySpec -> [FactorySpec] -> ([FactorySpec], [FactorySpec])
type NextReadyFactoryPoppedSpecs futureOutputs spec moreSpecs =
  CheckMakingProgress
    (UncheckedNextReadyFactoryPoppedSpecs futureOutputs spec moreSpecs)

type UncheckedNextReadyFactoryPoppedSpecs :: TypeSet -> FactorySpec -> [FactorySpec] -> ([FactorySpec], [FactorySpec])
type UncheckedNextReadyFactoryPoppedSpecs futureOutputs spec moreSpecs =
  GoNextReadyFactoryPoppedSpecs
    (Factory.FactoryInputs spec `TS.HasNoIntersectionWith` futureOutputs)
    futureOutputs
    '[]
    '[]
    spec
    moreSpecs

type CheckMakingProgress :: ([FactorySpec], [FactorySpec]) -> ([FactorySpec], [FactorySpec])
type family CheckMakingProgress specs where
  CheckMakingProgress '( '[], '[]) = '( '[], '[])
  CheckMakingProgress '( '[], leftoverSpecs) =
    -- If the list of ready factories in empty, we means we cannot make progress
    -- This must be cause by some dependency loop in the leftovers
    TypeError
      ( ListAllFactoryNamesForError
          ( 'Text "Cannot make progress due to cycle between factories")
          leftoverSpecs
      )
  CheckMakingProgress specs = specs

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
    HNil
    (spec ::: moreSpecs)

type GoPopNextReadyFactory ::
  Bool -> TypeSet -> [FactorySpec] -> [FactorySpec] -> FactorySpec -> [FactorySpec] -> (Type -> Type) -> Constraint
class
  GoPopNextReadyFactory
    specDoesNotNeedFutureOutput
    futureOutputs
    readySpecs
    previousSpecs
    spec
    moreSpecs
    m
  where
  type
    GoNextReadyFactoryPoppedSpecs
      specDoesNotNeedFutureOutput
      futureOutputs
      readySpecs
      previousSpecs
      spec
      moreSpecs ::
      ([FactorySpec], [FactorySpec])
  goPopNextReadyFactory ::
    HList (Factories m readySpecs) ->
    HList (Factories m previousSpecs) ->
    HList (Factories m (spec ': moreSpecs)) ->
    NextReadyFactoriesPopped
      m
      (GoNextReadyFactoryPoppedSpecs specDoesNotNeedFutureOutput futureOutputs readySpecs previousSpecs spec moreSpecs)

type NextReadyFactoriesPopped :: (Type -> Type) -> ([FactorySpec], [FactorySpec]) -> Type
type family NextReadyFactoriesPopped m poppedSpecs where
  NextReadyFactoriesPopped m '(readySpecs, moreSpecs) =
    (HList (Factories m readySpecs), HList (Factories m moreSpecs))

instance
  ( Factories m (Reverse previousSpecs ++ moreSpecs)
      ~ (Factories m (Reverse previousSpecs) ++ Factories m moreSpecs)
  , Factories m (Reverse previousSpecs) ~ Reverse (Factories m previousSpecs)
  , GoPopNextReadyFactory
      ( TS.HasNoIntersectionWith
          (Factory.FactoryInputs nextSpec)
          futureOutputs
      )
      futureOutputs
      (spec : readySpecs)
      previousSpecs
      nextSpec
      moreSpecs
      m
  ) =>
  GoPopNextReadyFactory
    'True
    futureOutputs
    readySpecs
    previousSpecs
    spec
    (nextSpec : moreSpecs)
    m
  where
  type
    GoNextReadyFactoryPoppedSpecs
      'True
      futureOutputs
      readySpecs
      previousSpecs
      spec
      (nextSpec : moreSpecs) =
      GoNextReadyFactoryPoppedSpecs
        ( TS.HasNoIntersectionWith
            (Factory.FactoryInputs nextSpec)
            futureOutputs
        )
        futureOutputs
        (spec : readySpecs)
        previousSpecs
        nextSpec
        moreSpecs

  {-# INLINE goPopNextReadyFactory #-}
  goPopNextReadyFactory ready previous (factory ::: more) =
    -- goPopNextReadyFactory (factory ::: ready) previous more
    goPopNextReadyFactory
      @(Factory.FactoryInputs nextSpec `TS.HasNoIntersectionWith` futureOutputs)
      @futureOutputs
      (factory ::: ready)
      previous
      more

instance
  ( Factories m (HList.Reverse previousSpecs)
      ~ HList.Reverse (Factories m previousSpecs)
  , HList.HReverse (Factories m previousSpecs)
  , Factories m (HList.Reverse (spec : readySpecs))
      ~ HList.Reverse (Factory m spec : Factories m readySpecs)
  , HList.HReverse (Factory m spec : Factories m readySpecs)
  ) =>
  GoPopNextReadyFactory
    'True
    futureOutputs
    readySpecs
    previousSpecs
    spec
    '[]
    m
  where
  type
    GoNextReadyFactoryPoppedSpecs
      'True
      futureOutputs
      readySpecs
      previousSpecs
      spec
      '[] =
      '(HList.Reverse (spec : readySpecs), Reverse previousSpecs)
  {-# INLINE goPopNextReadyFactory #-}
  goPopNextReadyFactory ready previous (factory ::: HNil) =
    (HList.hReverse (factory ::: ready), HList.hReverse previous)

instance
  ( Factories m (HList.Reverse (spec : previousSpecs))
      ~ HList.Reverse
          (Factory.Factory m spec : Factories m previousSpecs)
  , HList.HReverse (Factory.Factory m spec : Factories m previousSpecs)
  , Factories m (HList.Reverse readySpecs)
      ~ HList.Reverse (Factories m readySpecs)
  , HList.HReverse (Factories m readySpecs)
  ) =>
  GoPopNextReadyFactory
    'False
    futureOutputs
    readySpecs
    previousSpecs
    spec
    '[]
    m
  where
  type
    GoNextReadyFactoryPoppedSpecs
      'False
      futureOutputs
      readySpecs
      previousSpecs
      spec
      '[] =
      '(HList.Reverse readySpecs, Reverse (spec : previousSpecs))
  {-# INLINE goPopNextReadyFactory #-}
  goPopNextReadyFactory ready previous (factory ::: HNil) =
    (HList.hReverse ready, HList.hReverse (factory ::: previous))

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
      readySpecs
      (spec : previousSpecs)
      nextSpec
      moreSpecs
      m
  ) =>
  GoPopNextReadyFactory
    'False
    futureOutputs
    readySpecs
    previousSpecs
    spec
    (nextSpec : moreSpecs)
    m
  where
  type
    GoNextReadyFactoryPoppedSpecs
      'False
      futureOutputs
      readySpecs
      previousSpecs
      spec
      (nextSpec : moreSpecs) =
      GoNextReadyFactoryPoppedSpecs
        (Factory.FactoryInputs nextSpec `TS.HasNoIntersectionWith` futureOutputs)
        futureOutputs
        readySpecs
        (spec : previousSpecs)
        nextSpec
        moreSpecs
  {-# INLINE goPopNextReadyFactory #-}
  goPopNextReadyFactory ready previous (factory ::: more) =
    goPopNextReadyFactory
      @(Factory.FactoryInputs nextSpec `TS.HasNoIntersectionWith` futureOutputs)
      @futureOutputs
      ready
      (factory ::: previous)
      more

type ListAllFactoryNamesForError :: ErrorMessage -> [FactorySpec] -> ErrorMessage
type family ListAllFactoryNamesForError prefix specs where
  ListAllFactoryNamesForError prefix '[] = prefix
  ListAllFactoryNamesForError prefix (spec ': specs) =
    ListAllFactoryNamesForError (prefix ':$$: 'Text (Factory.FactoryName spec)) specs
