{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling2Spec (spec) where

import qualified Bundling2.Exports as B2
import Bundling2.Some (Some (MkSome))
import Data.Function ((&))
import Data.Functor ((<&>))
import Test.Hspec

spec :: Spec
spec = do
  let exports =
        mempty
          & B2.addExport @Foo 42
          & B2.addExport @Quux (MkSome [(), (), ()])
  describe "get things from a bundle" do
    it "can get a Foo" do
      B2.getExport @Foo exports `shouldBe` Just 42
    it "can try to get a Bar but fail" do
      B2.getExport @Bar exports `shouldBe` Nothing
    it "can get a Quux" do
      ( B2.getExport @Quux exports <&> \case
          MkSome q -> show q
        )
        `shouldBe` Just "[(),(),()]"

data Foo
  deriving (B2.ExportName) via (B2.Export Word)

data Bar
  deriving (B2.ExportName) via (B2.Export Int)

data Quux
  deriving (B2.ExportName) via (B2.Export (Some Show))
