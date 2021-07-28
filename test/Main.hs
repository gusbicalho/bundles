{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import qualified Bundling as B
import qualified Bundling.TypeSet as TS
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum (Sum))
import HList (HList (HNil, (:::)))
import Numeric.Natural (Natural)
import Test.Hspec

main :: IO ()
main = hspec do
  describe "manually assembled in IO" do
    it "assembles correctly" $
      ( pure @IO []
          >>= B.addFromFactory (blaFactory 42)
          >>= B.addFromFactory (blaFactory 17)
          >>= B.addFromFactory fooFactory
          >>= B.assemble (B.collector @Bla ::: B.folder @Bla ::: B.folder @[Foo] ::: HNil)
      )
        `shouldReturn` [Bla 42, Bla 17] ::: (Bla 59 ::: ([FooN 0, FooS "", FooB (Bla 59)] ::: HNil))
  describe "assembled Setup" do
    it "correctly in IO" do
      let assemblers = B.collector @Bla ::: B.folder @Bla ::: B.folder @[Foo] ::: HNil
          factories = blaFactory 42 ::: blaFactory 17 ::: fooFactory ::: HNil
      B.assembleSetup assemblers factories
        `shouldReturn` [Bla 42, Bla 17] ::: (Bla 59 ::: ([FooN 0, FooS "", FooB (Bla 59)] ::: HNil))
    it "correctly purely" do
      let assemblers = B.collector @Bla ::: B.folder @Bla ::: B.folder @[Foo] ::: HNil
          factories = blaFactory 42 ::: blaFactory 17 ::: fooFactory ::: HNil
      B.assemblePureSetup assemblers factories
        `shouldBe` [Bla 42, Bla 17] ::: (Bla 59 ::: ([FooN 0, FooS "", FooB (Bla 59)] ::: HNil))

newtype Bla = Bla Natural
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via (Sum Natural)

blaFactory ::
  Applicative m =>
  Natural ->
  B.Factory
    m
    ( 'B.FactorySpec
        "bla"
        [Char]
        TS.Empty
        (TS.FromList '[Bla])
    )
blaFactory nat = B.standalone @"bla" $ B.Bundle "bla" (Just (Bla nat) ::: HNil)

data Foo
  = FooN Natural
  | FooS String
  | FooB Bla
  deriving stock (Eq, Show)

fooFactory ::
  Applicative m =>
  B.Factory
    m
    ( 'B.FactorySpec
        "foo"
        String
        (TS.FromList '[Word, Char, Bla])
        (TS.FromList '[[Foo]])
    )
fooFactory = B.factoryPure (pure . build . Foldable.foldl' collect empty)
 where
  empty = ([], [], mempty :: Bla)
  collect (ws, cs, bla) (B.Bundle _ (w ::: c ::: b ::: HNil)) =
    (ws <> Foldable.toList w, cs <> Foldable.toList c, bla <> Maybe.fromMaybe mempty b)
  build (ws, cs, bla) =
    B.Bundle
      "foo.bundle"
      ( Just
          [ FooN (sum (fromIntegral @_ @Natural <$> ws))
          , FooS cs
          , FooB bla
          ]
          ::: HNil
      )

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
