{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Massiv.Core
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core
  ( Array
  , Construct(..)
  , Source(..)
  , Load(..)
  , Shape(..)
  , Slice(..)
  , Manifest(..)
  , Mutable(..)
  -- * Computation
#if __GLASGOW_HASKELL__ >= 800
  , Comp(Seq, Par, ParOn)
  , pattern Par
#else
  , Comp(..)
  , pattern Par
#endif
  , evaluateAt
  , module Data.Massiv.Core.Index
  , module Data.Massiv.Core.Iterator
  ) where

import           Control.Monad.Primitive      (PrimMonad (..))
import           Control.Monad.ST             (ST)
import           Data.Massiv.Core.Computation
import           Data.Massiv.Core.Index
import           Data.Massiv.Core.Iterator
import           Data.Proxy
import           Data.Typeable                (Typeable, showsTypeRep, typeRep)


data family Array r ix e :: *


-- | Index polymorphic arrays.
class (Typeable r, Index ix) => Construct r ix e where

  size :: Array r ix e -> ix

  getComp :: Array r ix e -> Comp

  setComp :: Comp -> Array r ix e -> Array r ix e

  unsafeMakeArray :: Comp -> ix -> (ix -> e) -> Array r ix e


class Construct r ix e => Source r ix e where

  unsafeIndex :: Array r ix e -> ix -> e
  unsafeIndex !arr = unsafeLinearIndex arr . toLinearIndex (size arr)
  {-# INLINE unsafeIndex #-}

  unsafeLinearIndex :: Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}


class Construct r ix e => Load r ix e where
  -- | Load an array into memory sequentially
  loadS
    :: Array r ix e -- ^ Array that is being loaded
    -> (Int -> ST s e) -- ^ Function that reads an element from target array
    -> (Int -> e -> ST s ()) -- ^ Function that writes an element into target array
    -> ST s ()

  -- | Load an array into memory in parallel
  loadP
    :: [Int] -- ^ List of capabilities to run workers on, as described in
             -- `Control.Concurrent.forkOn`. Empty list will imply all
             -- capabilities, i.e. run on all cores available through @+RTS -N@.
    -> Array r ix e -- ^ Array that is being loaded
    -> (Int -> IO e) -- ^ Function that reads an element from target array
    -> (Int -> e -> IO ()) -- ^ Function that writes an element into target array
    -> IO ()


class (Source r ix e, Source (R r) ix e) => Shape r ix e where
  type R r :: *
  type R r = r

  unsafeReshape :: Index ix' => ix' -> Array r ix e -> Array r ix' e

  unsafeExtract :: r' ~ R r => ix -> ix -> Array r ix e -> Array r' ix e


class ( Index (Lower ix)
      , Shape r ix e
      , Shape (R r) ix e
      , Shape (R r) (Lower ix) e
      ) =>
      Slice r ix e where
  (!?>) :: Array r ix e -> Int -> Maybe (Array (R r) (Lower ix) e)
  (<!?) :: Array r ix e -> Int -> Maybe (Array (R r) (Lower ix) e)


-- | Manifest arrays are backed by actual memory and values are looked up versus
-- computed as it is with delayed arrays. Because of this fact indexing functions
-- `(!)`, `(!?)`, etc. are constrained to manifest arrays only.
class Source r ix e => Manifest r ix e where

  unsafeLinearIndexM :: Array r ix e -> Int -> e


class Manifest r ix e => Mutable r ix e where
  data MArray s r ix e :: *

  -- | Get the size of a mutable array.
  msize :: MArray s r ix e -> ix

  unsafeThaw :: PrimMonad m =>
                Array r ix e -> m (MArray (PrimState m) r ix e)

  unsafeFreeze :: PrimMonad m =>
                  Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)

  unsafeNew :: PrimMonad m =>
               ix -> m (MArray (PrimState m) r ix e)

  unsafeLinearRead :: PrimMonad m =>
                      MArray (PrimState m) r ix e -> Int -> m e

  unsafeLinearWrite :: PrimMonad m =>
                       MArray (PrimState m) r ix e -> Int -> e -> m ()



-- TODO: Implement proper Show
instance (Typeable e, Construct r ix e) => Show (Array r ix e) where
  show arr =
    "<Array " ++
    showsTypeRep (typeRep (Proxy :: Proxy r)) " " ++
    (show (size arr)) ++ " (" ++ showsTypeRep (typeRep (Proxy :: Proxy e)) ")>"



-- | This is just like safe `Data.Massiv.Array.Manifest.index` function, but it
-- allows getting values from delayed arrays as well as manifest. As the name
-- suggests, indexing into a delayed array at the same index multiple times will
-- cause evaluation of the value each time.
evaluateAt :: Source r ix e => Array r ix e -> ix -> e
evaluateAt !arr !ix =
  handleBorderIndex
    (Fill (errorIx "evaluateAt" (size arr) ix))
    (size arr)
    (unsafeIndex arr)
    ix
{-# INLINE evaluateAt #-}
