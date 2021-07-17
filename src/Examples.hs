{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Examples where

import Bundling qualified as B
import Bundling.TypeSet qualified as TS
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Maybe qualified as Maybe
import Data.Monoid (Sum (Sum))
import Data.Typeable (Typeable)
import Debug.Trace qualified as Trace
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
blaFactory nat = B.standalone @"bla" "bla" (Just (Bla nat) ::: HNil)

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

collectAll :: forall t. Typeable t => B.Assembler String [t]
collectAll = B.assembler (Foldable.foldMap $ \(B.Bundle meta (e ::: HNil)) -> Trace.trace ("meta: " <> meta) `seq` Maybe.maybeToList e)

mappendAll :: forall t. (Typeable t, Monoid t) => B.Assembler String t
mappendAll = B.assembler (Foldable.foldMap $ \(B.Bundle _ (e ::: HNil)) -> Maybe.fromMaybe mempty e)

assembled :: HList '[[Bla], Bla]
assembled = B.assemble (collectAll @Bla ::: mappendAll @Bla ::: HNil) bundles

-- >>> assembled
-- [Bla 42,Bla 17,Bla 59] ::: (Bla 118 ::: HNil)
