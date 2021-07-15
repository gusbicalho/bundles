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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{- |
Copyright: (c) 2021 Gustavo Bicalho
SPDX-License-Identifier: MIT
Maintainer: Gustavo Bicalho <gusbicalho@gmail.com>

See README for more info
-}
module Bundling where

import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Data.Foldable qualified as Foldable
import Data.Kind (Constraint, Type)
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, typeRep)
import GHC.Base (Any)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import HList (HList (HNil, (:::)))
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

type MapTypes :: forall k l. (k -> l) -> [k] -> [l]
type family MapTypes f types where
  MapTypes _ '[] = '[]
  MapTypes f (t ': ts) = f t ': MapTypes f ts

type Maybes :: [Type] -> [Type]
type Maybes types = MapTypes Maybe types

type BundleMeta = String
type RepMap = [(TypeRep, Any)]

lookupT :: forall t. (Typeable t) => RepMap -> Maybe t
lookupT repMap =
  case lookup (typeRep (Proxy @t)) repMap of
    Nothing -> Nothing
    Just a -> Just (unsafeCoerce a :: t)

data DynBundle = DynBundle
  { bundleMeta :: BundleMeta
  , bundleExports :: RepMap
  }

data Bundle types = Bundle BundleMeta (HList (Maybes types))

getExport :: forall t. (Typeable t) => DynBundle -> Maybe t
getExport (DynBundle _ exports) = lookupT @t exports

data FactorySpec
  = FactorySpec Symbol TypeSet TypeSet

newtype Factory (spec :: FactorySpec)
  = Factory ([DynBundle] -> [DynBundle])

class ToRepMap t where
  toRepMap :: t -> RepMap

instance ToRepMap (HList '[]) where
  toRepMap HNil = []

type AllUniqueTypes :: [Type] -> Constraint
type AllUniqueTypes types = types ~ TS.Elements (TS.FromList types)

instance (Typeable t, ToRepMap (HList ts)) => ToRepMap (HList (t ': ts)) where
  toRepMap (a ::: more) =
    (typeRep (Proxy @t), unsafeCoerce a) : toRepMap more

class FromRepMap t where
  fromRepMap :: RepMap -> t

instance FromRepMap (HList '[]) where
  fromRepMap _ = HNil

instance (t ~ Maybe u, Typeable u, FromRepMap (HList ts)) => FromRepMap (HList (t ': ts)) where
  fromRepMap repMap = lookupT @u repMap ::: fromRepMap @(HList ts) repMap

type HListOfInputs t inputs =
  ( t ~ HList (Maybes inputs)
  , AllUniqueTypes inputs
  , FromRepMap t
  )

type ValidInputs inputs = HListOfInputs (HList (Maybes inputs)) inputs

type HListOfOutputs t outputs =
  ( t ~ HList (Maybes outputs)
  , AllUniqueTypes outputs
  , ToRepMap t
  )

type ValidOutputs outputs = HListOfOutputs (HList (Maybes outputs)) outputs

standalone ::
  forall name types t.
  (HListOfOutputs t types, KnownSymbol name) =>
  t ->
  Factory ( 'FactorySpec name TS.Empty (TS.FromList types))
standalone feats = Factory $ const [DynBundle meta exports]
 where
  meta = symbolVal (Proxy @name) <> ".bundle"
  exports = toRepMap feats

factory ::
  forall name inputs outputs.
  ( ValidInputs inputs
  , ValidOutputs outputs
  ) =>
  ([Bundle inputs] -> [Bundle outputs]) ->
  Factory ( 'FactorySpec name (TS.FromList inputs) (TS.FromList outputs))
factory runFactory = Factory go
 where
  go bundles = toBundle <$> runFactory (fromBundle <$> bundles)
  toBundle (Bundle meta exports) = DynBundle meta (toRepMap exports)
  fromBundle DynBundle{bundleMeta, bundleExports} = Bundle bundleMeta (fromRepMap bundleExports)

fooFactory ::
  Factory
    ( 'FactorySpec
        "foo"
        (TS.FromList '[Word, Char, [Char]])
        (TS.FromList '[Natural, [Char], [String]])
    )
fooFactory = factory (pure . build . Foldable.foldl' collect empty)
 where
  empty = ([], [], [])
  collect (ws, cs, strs) (Bundle _ (w ::: c ::: s ::: HNil)) =
    (ws <> Foldable.toList w, cs <> Foldable.toList c, strs <> Foldable.toList s)
  build (ws, cs, strs) =
    Bundle
      "foo.bundle"
      ( Just (sum (fromIntegral @_ @Natural <$> ws))
          ::: Just cs
          ::: Just strs
          ::: HNil
      )
