{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

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
blaFactory nat = B.standalone @"bla" $ Bundle "bla" (Just (Bla nat) ::: HNil)

data Foo
  = FooN Natural
  | FooS String
  | FooB Bla
  deriving stock (Show)

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

manuallyAssembled :: Monad m => m (HList '[[Bla], Bla, [Foo]])
manuallyAssembled =
  pure []
    >>= B.addFromFactory (blaFactory 42)
    >>= B.addFromFactory (blaFactory 17)
    >>= B.addFromFactory fooFactory
    >>= B.assemble (B.collector @Bla ::: B.folder @Bla ::: B.folder @[Foo] ::: HNil)

-- >>> manuallyAssembled @IO
-- [Bla 42,Bla 17] ::: (Bla 59 ::: ([FooN 0,FooS "",FooB (Bla 59)] ::: HNil))

assembledSetup :: Monad m => m (HList '[[Bla], Bla, [Foo]])
assembledSetup = do
  bundles <-
    B.runSetup [] (blaFactory 42 B.:>> blaFactory 17 B.:>> fooFactory B.:>> B.Empty)
  B.assemble
    (B.collector @Bla ::: B.folder @Bla ::: B.folder @[Foo] ::: HNil)
    bundles

-- >>> assembledSetup @IO
-- [Bla 42,Bla 17] ::: (Bla 59 ::: ([FooN 0,FooS "",FooB (Bla 59)] ::: HNil))

autoSetup :: Monad m => m (HList '[[Bla], Bla, [Foo]])
autoSetup = B.assembleSetup assemblers factories
 where
  assemblers = B.collector @Bla ::: B.folder @Bla ::: B.folder @[Foo] ::: HNil
  factories = blaFactory 42 ::: blaFactory 17 ::: fooFactory ::: HNil

-- >>> autoSetup @IO
-- [Bla 42,Bla 17] ::: (Bla 59 ::: ([FooN 0,FooS "",FooB (Bla 59)] ::: HNil))

autoSetupPure :: HList '[[Bla], Bla, [Foo]]
autoSetupPure = B.assemblePureSetup assemblers factories
 where
  assemblers = B.collector @Bla ::: B.folder @Bla ::: B.folder @[Foo] ::: HNil
  factories = blaFactory 42 ::: blaFactory 17 ::: fooFactory ::: HNil

-- >>> autoSetupPure
-- [Bla 42,Bla 17] ::: (Bla 59 ::: ([FooN 0,FooS "",FooB (Bla 59)] ::: HNil))

-- >>> import Data.Functor.Identity (Identity)
-- >>> autoSetup @Identity
-- Identity ([Bla 42,Bla 17] ::: (Bla 59 ::: ([FooN 0,FooS "",FooB (Bla 59)] ::: HNil)))

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
        pure
          [ (\s -> Bundle "fromNatToInt.bundle" (s ::: HNil))
              . getSum
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
          [ (\s -> Bundle "fromIntToNat.bundle" (s ::: HNil))
              . getSum
              . Foldable.foldMap (\(nat ::: HNil) -> Sum (fromIntegral nat))
              . fmap B.bundleExports
              $ bs
          ]
-- -}
