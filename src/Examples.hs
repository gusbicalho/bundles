{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Examples where

import Bundling qualified as B
import Bundling.Bundle (Bundle (Bundle))
import Bundling.TypeSet qualified as TS
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Maybe qualified as Maybe
import Data.Monoid (Sum (Sum))
import HList (HList (HNil, (:::)))
import Numeric.Natural (Natural)

newtype Bla = Bla Natural
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Sum Natural)

blaFactory ::
  Natural ->
  B.Factory
    ( 'B.FactorySpec
        "bla"
        [Char]
        TS.Empty
        (TS.FromList '[Bla])
    )
blaFactory nat = B.standalone @"bla" $ Bundle "bla" (Just (Bla nat) ::: HNil)

fooFactory ::
  B.Factory
    ( 'B.FactorySpec
        "foo"
        String
        (TS.FromList '[Word, Char, Bla])
        (TS.FromList '[Natural, [Char], Bla])
    )
fooFactory = B.factory (pure . build . Foldable.foldl' collect empty)
 where
  empty = ([], [], mempty :: Bla)
  collect (ws, cs, bla) (B.Bundle _ (w ::: c ::: b ::: HNil)) =
    (ws <> Foldable.toList w, cs <> Foldable.toList c, bla <> Maybe.fromMaybe mempty b)
  build (ws, cs, strs) =
    B.Bundle
      "foo.bundle"
      ( Just (sum (fromIntegral @_ @Natural <$> ws))
          ::: Just cs
          ::: Just strs
          ::: HNil
      )

bundles :: [B.DynamicBundle [Char]]
bundles =
  [] & B.addFromFactory (blaFactory 42) & B.addFromFactory (blaFactory 17) & B.addFromFactory fooFactory

assembled :: HList '[[Bla], Bla]
assembled = B.assemble (B.collector @Bla ::: B.folder @Bla ::: HNil) bundles

-- >>> assembled
-- [Bla 42,Bla 17,Bla 59] ::: (Bla 118 ::: HNil)

assembledSetup :: HList '[[Bla], Bla]
assembledSetup =
  B.assembleSetup
    (B.collector @Bla ::: B.folder @Bla ::: HNil)
    (blaFactory 42 B.:>> blaFactory 17 B.:>> fooFactory B.:>> B.Empty)

-- >>> assembledSetup
-- [Bla 42,Bla 17,Bla 59] ::: (Bla 118 ::: HNil)
