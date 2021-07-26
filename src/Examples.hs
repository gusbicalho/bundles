{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Examples where

import Bundling (Bundle (Bundle))
import Bundling qualified as B
import Bundling.TypeSet qualified as TS
import Data.Foldable qualified as Foldable
import Data.Maybe qualified as Maybe
import Data.Monoid (Sum (..))
import HList (HList (HNil, (:::)))
import Numeric.Natural (Natural)

newtype Bla = Bla Natural
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Sum Natural)

blaFactory ::
  Natural ->
  B.Factory
    _
    ( 'B.FactorySpec
        "bla"
        [Char]
        TS.Empty
        (TS.FromList '[Bla])
    )
blaFactory nat = B.standalone @"bla" $ Bundle "bla" (Just (Bla nat) ::: HNil)

data Foo
  = FooN Natural
  | FooS String
  | FooB Bla
  deriving stock (Show)

fooFactory ::
  B.Factory
    _
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

manuallyAssembled :: IO (HList '[[Bla], Bla, [Foo]])
manuallyAssembled =
  pure []
    >>= B.addFromFactory (blaFactory 42)
    >>= B.addFromFactory (blaFactory 17)
    >>= B.addFromFactory fooFactory
    >>= B.assemble (B.collector @Bla ::: B.folder @Bla ::: B.folder @[Foo] ::: HNil)

-- >>> manuallyAssembled
-- [Bla 42,Bla 17] ::: (Bla 59 ::: ([FooN 0,FooS "",FooB (Bla 59)] ::: HNil))

assembledSetup :: IO (HList '[[Bla], Bla, [Foo]])
assembledSetup = do
  bundles <-
    B.runSetup [] (blaFactory 42 B.:>> blaFactory 17 B.:>> fooFactory B.:>> B.Empty)
  B.assemble
    (B.collector @Bla ::: B.folder @Bla ::: B.folder @[Foo] ::: HNil)
    bundles

-- >>> assembledSetup
-- [Bla 42,Bla 17] ::: (Bla 59 ::: ([FooN 0,FooS "",FooB (Bla 59)] ::: HNil))

autoSetup :: IO (HList '[[Bla], Bla, [Foo]])
autoSetup = B.assembleSetup assemblers factories
 where
  assemblers = B.collector @Bla ::: B.folder @Bla ::: B.folder @[Foo] ::: HNil
  factories = blaFactory 42 ::: blaFactory 17 ::: fooFactory ::: HNil

-- >>> autoSetup
-- [Bla 42,Bla 17] ::: (Bla 59 ::: ([FooN 0,FooS "",FooB (Bla 59)] ::: HNil))

{-
-- broken due to cycle
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
        [ bs
            & fmap B.bundleExports
            & Foldable.foldMap (\(nat ::: HNil) -> Sum (fromIntegral nat))
            & getSum
            & \s -> Bundle "fromNatToInt.bundle" (s ::: HNil)
        ]
  fromIntToNat =
    B.factory
      @"fromIntToNat"
      @(TS.FromList '[Integer])
      @(TS.FromList '[Natural])
      $ \bs ->
          [ bs
              & fmap B.bundleExports
              & Foldable.foldMap (\(nat ::: HNil) -> Sum (fromIntegral nat))
              & getSum
              & \s -> Bundle "fromIntToNat.bundle" (s ::: HNil)
          ]
-- -}
