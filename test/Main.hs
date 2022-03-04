{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Test.Hspec
import BundlingSpec qualified
import Bundling2Spec qualified

main :: IO ()
main = hspec $ do
  describe "BundlingSpec" BundlingSpec.spec
  describe "Bundling2Spec" Bundling2Spec.spec

{-
-- This example is broken due to cycle, which reports this error:
-- • Cannot make progress due to cycle between factories
--   fromNatToInt
--   fromIntToNat
-- • When checking the inferred type ...
brokenSetup = B.buildSetup (blaFactory 42 ::: fromNatToInt ::: fooFactory ::: fromIntToNat ::: HNil)
 where
  fromNatToInt =
    B.factory
      @"fromNatToInt"
      @(TS.FromList '[Natural])
      @(TS.FromList '[Integer])
      $ \bs ->
        pure
          [ (\s -> B.Bundle "fromNatToInt.bundle" (s ::: HNil))
              . (\(Sum s) -> s)
              . Foldable.foldMap (\(nat ::: HNil) -> Sum (fromIntegral nat))
              . fmap B.bundleExports
              $ bs
          ]
  fromIntToNat =
    B.factory
      @"fromIntToNat"
      @(TS.FromList '[Integer])
      @(TS.FromList '[Natural])
      $ \bs ->
        pure
          [ (\s -> B.Bundle "fromIntToNat.bundle" (s ::: HNil))
              . (\(Sum s) -> s)
              . Foldable.foldMap (\(nat ::: HNil) -> Sum (fromIntegral nat))
              . fmap B.bundleExports
              $ bs
          ]
-- -}
