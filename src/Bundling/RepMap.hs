{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Bundling.RepMap (
  RepMap,
  Bundling.RepMap.lookup,
  empty,
  insert,
  size,
  ToRepMap (..),
  FromRepMap (..),
) where

import Data.Foldable qualified as Foldable
import Data.Maybe (Maybe (Just, Nothing))
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Exts (Any)
import HList (HList (HNil, (:::)))
import Unsafe.Coerce (unsafeCoerce)
import Prelude (Int, otherwise, (==))
import Prelude qualified

newtype RepMap = MkRepMap [(TypeRep, Any)]

empty :: RepMap
empty = MkRepMap []

lookup :: forall t. (Typeable t) => RepMap -> Maybe t
lookup (MkRepMap repMap) =
  case Prelude.lookup (typeRep (Proxy @t)) repMap of
    Nothing -> Nothing
    Just a -> Just (unsafeCoerce a :: t)

insert :: forall t. Typeable t => t -> RepMap -> RepMap
insert v (MkRepMap repMap) = MkRepMap (go repMap)
 where
  rep = typeRep (Proxy @t)
  go [] = [(rep, unsafeCoerce v)]
  go (entry@(rep', _) : more)
    | rep' == rep = (rep, unsafeCoerce v) : more
    | otherwise = entry : go more

size :: RepMap -> Int
{-# INLINE size #-}
size (MkRepMap repMap) = Foldable.length repMap

class ToRepMap t where
  toRepMap :: t -> RepMap

class FromRepMap t where
  fromRepMap :: RepMap -> t

instance ToRepMap (HList '[]) where
  {-# INLINE toRepMap #-}
  toRepMap HNil = empty

instance (t ~ Maybe u, Typeable u, ToRepMap (HList ts)) => ToRepMap (HList (t ': ts)) where
  {-# INLINE toRepMap #-}
  toRepMap (a ::: more) = case a of
    Nothing -> toRepMap more
    Just a' -> insert a' (toRepMap more)

instance FromRepMap (HList '[]) where
  {-# INLINE fromRepMap #-}
  fromRepMap _ = HNil

instance (t ~ Maybe u, Typeable u, FromRepMap (HList ts)) => FromRepMap (HList (t ': ts)) where
  {-# INLINE fromRepMap #-}
  fromRepMap repMap = lookup @u repMap ::: fromRepMap @(HList ts) repMap
