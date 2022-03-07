{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Bundling2.Bundle where

import Bundling2.DynBag (DynBag, Some)
import Data.Foldable (toList)
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH (Q, TExp)
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)
import qualified Bundling2.DynBag as DynBag

data Bundle meta = Bundle meta (Some DynBag)

type Code a = Q (TExp a)

type FactoryCode meta = Code ([Bundle meta] -> [Bundle meta])

data Factory meta bundleMeta = Factory
  { factoryName :: String
  , factoryMeta :: meta
  , factoryInputs :: Set SomeTypeRep
  , factoryOutputs :: Set SomeTypeRep
  , factoryCode :: FactoryCode bundleMeta
  }

-- Builder

newtype BuildsAfter = MkBuildsAfter {getBuildsAfter :: Map String (Set String)}
  deriving stock (Eq, Ord, Show)

buildsAfter :: String -> String -> BuildsAfter
consumer `buildsAfter` provider =
  MkBuildsAfter (Map.singleton consumer (Set.singleton provider))

isConsumerSatisfied :: BuildsAfter -> Set String -> String -> Bool
isConsumerSatisfied (MkBuildsAfter buildsAfter') todo consumer =
  case Map.lookup consumer buildsAfter' of
    Nothing -> True
    Just buildsAfterProviders -> Set.disjoint buildsAfterProviders todo

instance Semigroup BuildsAfter where
  MkBuildsAfter r1 <> MkBuildsAfter r2 = MkBuildsAfter $ Map.unionWith (<>) r1 r2

instance Monoid BuildsAfter where
  mempty = MkBuildsAfter Map.empty

data Err meta
  = FactoryNameConflict String meta meta
  | DependencyCycle [String]
  deriving stock (Eq, Ord, Show)

factoryRunner :: [Factory meta bundleMeta] -> Either (Err meta) (FactoryCode bundleMeta)
factoryRunner factories = do
  byName <- collectFactories factories
  let buildsAfterDeps = buildsAfterDependencies byName
  runFactories byName buildsAfterDeps

collectFactories :: [Factory meta bundleMeta] -> Either (Err meta) (Map String (Factory meta bundleMeta))
collectFactories = collectByM factoryName id onConflict
 where
  onConflict factoryName f1 f2 =
    Left $
      FactoryNameConflict
        factoryName
        (factoryMeta f1)
        (factoryMeta f2)

buildsAfterDependencies :: Map String (Factory meta bundleMeta) -> BuildsAfter
buildsAfterDependencies byName = foldMap factoryRequires $ factories
 where
  factories = Map.elems byName
  factoryRequires :: Factory meta bundleMeta -> BuildsAfter
  factoryRequires Factory{factoryName, factoryInputs, factoryOutputs} =
    (<>)
      ( foldMap
          ( \input -> case Map.lookup input byProvidedOutput of
              Nothing -> mempty
              Just providers -> foldMap (factoryName `buildsAfter`) providers
          )
          factoryInputs
      )
      ( foldMap
          ( \output -> case Map.lookup output byRequiredInput of
              Nothing -> mempty
              Just consumers -> foldMap (`buildsAfter` factoryName) consumers
          )
          factoryOutputs
      )
  byRequiredInput :: Map SomeTypeRep (NonEmpty String)
  byRequiredInput =
    collectBy fst ((:| []) . snd) (const (<>))
      . foldMap (\Factory{factoryName, factoryInputs} -> Set.map (,factoryName) factoryInputs)
      $ factories
  byProvidedOutput :: Map SomeTypeRep (NonEmpty String)
  byProvidedOutput =
    collectBy fst ((:| []) . snd) (const (<>))
      . foldMap (\Factory{factoryName, factoryOutputs} -> Set.map (,factoryName) factoryOutputs)
      $ factories

runFactories :: forall meta bundleMeta. Map String (Factory meta bundleMeta) -> BuildsAfter -> Either (Err meta) (FactoryCode bundleMeta)
runFactories byName buildsAfterRels = goRun (Map.keysSet byName)
 where
  goRun :: Set String -> Either (Err meta) (FactoryCode bundleMeta)
  goRun todo
    | Set.null todo = pure [||const []||]
    | otherwise = do
      (toRun, todo) <- nextRunnable todo
      runMore <- goRun todo
      pure $ case Map.lookup toRun byName of
        Nothing -> runMore
        Just Factory{factoryCode, factoryOutputs} ->
          let fouts = toList factoryOutputs
          in [||
          \bundles ->
            let moreBundles = enforceOutputs <$> $$factoryCode bundles
                enforceOutputs (Bundle meta bag) = Bundle meta (DynBag.restrict (Set.fromList fouts) bag)
             in $$runMore (moreBundles <> bundles)
          ||]
  nextRunnable :: Set String -> Either (Err meta) (String, Set String)
  nextRunnable todo =
    case Foldable.find (isConsumerSatisfied buildsAfterRels todo) todo of
      Nothing -> Left . DependencyCycle . toList $ todo
      Just s -> pure (s, Set.delete s todo)

-- Utilities

groupBy :: Ord b => (a -> b) -> [a] -> Map b (NonEmpty a)
groupBy getKey = collectBy getKey (:| []) (const (<>))

collectBy :: (Ord k, Foldable t) => (i -> k) -> (i -> a) -> (k -> a -> a -> a) -> t i -> Map k a
collectBy getKey getVal onConflict = runIdentity . collectByM getKey getVal (\k n o -> Identity $ onConflict k n o)

collectByM :: (Monad m, Ord k, Foldable t) => (i -> k) -> (i -> a) -> (k -> a -> a -> m a) -> t i -> m (Map k a)
collectByM getKey getVal onConflict = go Map.empty . toList
 where
  go acc = \case
    [] -> pure acc
    (x : xs) -> insertWithKeyM onConflict (getKey x) (getVal x) acc >>= flip go xs

insertWithKeyM :: (Monad m, Ord k) => (k -> a -> a -> m a) -> k -> a -> Map k a -> m (Map k a)
insertWithKeyM onConflict key newVal m = do
  conflictSolved <- case Map.lookup key m of
    Just existingVal -> onConflict key newVal existingVal
    Nothing -> pure newVal
  pure $ Map.insert key conflictSolved m
