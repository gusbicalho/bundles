{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling2Spec (spec) where

import Bundling2.Bundle qualified as B2
import Bundling2.DynBag qualified as DynBag
import Bundling2.Some (Some (MkSome), withSome)
import Data.Functor ((<&>))
import Data.Monoid (Sum (Sum, getSum))
import Test.Hspec

spec :: Spec
spec = do
  let exports =
        DynBag.fromDynamics
          [ DynBag.toDyn $ MkFoo 42
          , DynBag.toDyn $ MkFoo 100
          , DynBag.toDyn (MkQuux (MkSome [(), (), ()]))
          ]
  describe "get things from a bundle" do
    it "can get a Foo" do
      DynBag.getList @Foo exports `shouldBe` [MkFoo 42, MkFoo 100]
    it "can try to get a Bar but fail" do
      DynBag.getList @Bar exports `shouldBe` []
    it "can get a Quux" do
      ( DynBag.getList @Quux exports <&> \case
          MkQuux q -> withSome q show
        )
        `shouldBe` ["[(),(),()]"]

newtype Foo = MkFoo Word
  deriving stock (Eq, Show)

newtype Bar = MkBar Int
  deriving stock (Eq, Show)

newtype Quux = MkQuux (Some Show)
  deriving stock (Show)

fooToBar :: B2.Factory () ()
fooToBar =
  B2.Factory
    { B2.factoryName = "fooToBar"
    , B2.factoryMeta = ()
    , B2.factoryInputs = DynBag.typeReps @'[Foo]
    , B2.factoryOutputs = DynBag.typeReps @'[Bar]
    , B2.factoryCode = build
    }
 where
  build =
    [||
    \bundles ->
      let totalFoo = getSum $ foldMap bundleTotalFoo bundles
          bundleTotalFoo (B2.Bundle _ bag) =
            foldMap (\(MkFoo foo) -> Sum foo) $ DynBag.getList @Foo bag
       in (: []) . B2.Bundle () $
            DynBag.fromDynamics
              [ DynBag.toDyn $ MkBar (fromIntegral totalFoo)
              ]
    ||]

barAndFooToQuux :: B2.Factory () ()
barAndFooToQuux =
  B2.Factory
    { B2.factoryName = "barAndFooToQuux"
    , B2.factoryMeta = ()
    , B2.factoryInputs = DynBag.typeReps @'[Foo, Bar]
    , B2.factoryOutputs = DynBag.typeReps @'[Quux]
    , B2.factoryCode = build
    }
 where
  build =
    [||
    \bundles ->
      let totalFoo = getSum $ foldMap bundleTotalFoo bundles
          bundleTotalFoo (B2.Bundle _ bag) =
            foldMap (\(MkFoo foo) -> Sum foo) $ DynBag.getList @Foo bag
          totalBar = getSum $ foldMap bundleTotalBar bundles
          bundleTotalBar (B2.Bundle _ bag) =
            foldMap (\(MkBar foo) -> Sum foo) $ DynBag.getList @Bar bag
       in (: []) . B2.Bundle () $
            DynBag.fromDynamics
              [ DynBag.toDyn . MkQuux . MkSome $
                  "Foo: " <> show totalFoo <> ", Bar: " <> show totalBar
              ]
    ||]

runFactories :: Show meta => [B2.Factory meta bundleMeta] -> B2.FactoryCode bundleMeta
runFactories factories = case B2.factoryRunner factories of
  Left err -> error (show err)
  Right code -> code

-- runThem = $$(runFactories [fooToBar, barAndFooToQuux])
