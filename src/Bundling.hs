{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Bundling.Bundle (
  Bundle (Bundle, Exporting),
  GetExport (getExport),
  SomeBundle (..),
  TypeMapEntry ((:->)),
  someBundle,
 )
import Bundling.BundlingSetup (
  BundlingSetup,
  addBundle,
  addFactory,
  emptySetup,
 )
import Bundling.Factory (BundleFactory (..))
import Data.Function ((&))
import qualified Data.Maybe as Maybe
import Data.Text (Text)

bundleA ::
  Bundle
    "bundleA"
    "red"
    '["bar/name" ':-> Text, "foo/val" ':-> Word]
bundleA =
  Bundle @"bundleA" @"red"
    & Exporting @"foo/val" @Word 42
    & Exporting @"bar/name" @Text "Pretty foolish"

factoryFoo ::
  BundleFactory
    "factoryFoo"
    "blue"
    '["foo/val" ':-> Word, "foo/name" ':-> Text]
    '["bar/total" ':-> Word, "bar/names" ':-> [Text]]
factoryFoo = BundleFactory $ \inputBundles ->
  let total = inputBundles & Maybe.mapMaybe (\(SomeBundle b) -> getExport @"foo/val" @Word b) & sum
      names = inputBundles & Maybe.mapMaybe (\(SomeBundle b) -> getExport @"foo/name" @Text b)
   in [ someBundle $
          Bundle @"factoryFoo.bundle" @"blue"
            & Exporting @"bar/total" total
            & Exporting @"bar/names" names
      ]

runFactory :: BundleFactory name owner imports exports -> [SomeBundle imports] -> [SomeBundle exports]
runFactory (BundleFactory runF) = runF

assembled :: BundlingSetup _ _
assembled =
  emptySetup
    & addBundle bundleA
      . addFactory factoryFoo
