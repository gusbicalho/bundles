{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Examples where

import Bundling qualified as B
import Bundling.TypeSet qualified as TS
import Data.Foldable qualified as Foldable
import HList (HList (HNil, (:::)))
import Numeric.Natural (Natural)

fooFactory ::
  B.Factory
    ( 'B.FactorySpec
        "foo"
        String
        (TS.FromList '[Word, Char, [Char]])
        (TS.FromList '[Natural, [Char], [String]])
    )
fooFactory = B.factory (pure . build . Foldable.foldl' collect empty)
 where
  empty = ([], [], [])
  collect (ws, cs, strs) (B.Bundle _ (w ::: c ::: s ::: HNil)) =
    (ws <> Foldable.toList w, cs <> Foldable.toList c, strs <> Foldable.toList s)
  build (ws, cs, strs) =
    B.Bundle
      "foo.bundle"
      ( Just (sum (fromIntegral @_ @Natural <$> ws))
          ::: Just cs
          ::: Just strs
          ::: HNil
      )
