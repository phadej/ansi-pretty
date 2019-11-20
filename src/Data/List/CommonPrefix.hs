{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Data.List.CommonPrefix
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Data.List.CommonPrefix where

import Data.Data (Typeable, Data)
import Data.Semigroup
import GHC.Generics (Generic)

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

-- | Longest common prefix of lists.
newtype CommonPrefix a = CommonPrefix [a]
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Typeable, Data, Generic)

getCommonPrefix :: CommonPrefix a -> [a]
getCommonPrefix (CommonPrefix pfx) = pfx

instance Eq a => Data.Semigroup.Semigroup (CommonPrefix a) where
  CommonPrefix as <> CommonPrefix bs = CommonPrefix (impl as bs)
    where
      impl []     _       = []
      impl _      []      = []
      impl (x:xs) (y:ys)
        | x == y          = x : impl xs ys
        | otherwise       = []
