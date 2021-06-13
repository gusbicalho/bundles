{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Bundling.Factory (
  BundleFactory (..),
  BundleFactorySig (..),
  mapFactory,
  morphFactory,
) where

import Bundling.Bundle (MorphBundle, SomeBundle, morph)
import Bundling.TypeMap (TypeMap)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)

type BundleFactory :: Symbol -> Symbol -> TypeMap -> TypeMap -> Type
data BundleFactory name owner imports exports where
  BundleFactory ::
    forall name owner imports exports.
    ([SomeBundle imports] -> [SomeBundle exports]) ->
    BundleFactory name owner imports exports

data BundleFactorySig where
  BundleFactorySig :: Symbol -> Symbol -> TypeMap -> TypeMap -> BundleFactorySig

mapFactory ::
  ([SomeBundle exports1] -> [SomeBundle exports2]) ->
  BundleFactory name owner imports exports1 ->
  BundleFactory name owner imports exports2
mapFactory f (BundleFactory runF) = BundleFactory (f . runF)

morphFactory ::
  MorphBundle SomeBundle fromExports toExports =>
  BundleFactory name owner imports fromExports ->
  BundleFactory name owner imports toExports
morphFactory = mapFactory (fmap morph)
