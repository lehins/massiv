{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Core
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core
  ( Array(List, unList)
  , Elt
  , EltRepr
  , Construct(..)
  , Source(..)
  , Load(..)
  , Size(..)
  , Slice(..)
  , OuterSlice(..)
  , InnerSlice(..)
  , Manifest(..)
  , Mutable(..)
  , Ragged(..)
  , Nested(..)
  , L(..)
  , LN
  , ListItem
  -- * Computation
#if __GLASGOW_HASKELL__ >= 800
  , Comp(Seq, Par, ParOn)
  , pattern Par -- already exported above and only needed for Haddock
#else
  , Comp(..)
  , pattern Par
#endif
  , singleton
  , evaluateAt
  , module Data.Massiv.Core.Index
  , module Data.Massiv.Core.Iterator
  ) where

import           Control.Monad.Primitive      (PrimMonad (..))
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.Computation
import           Data.Massiv.Core.Index
import           Data.Massiv.Core.Iterator
import           Data.Massiv.Core.List
import           Data.Proxy
import           Data.Typeable

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


-- | Create an Array with a single element.
singleton :: Construct r ix e =>
             Comp -- ^ Computation strategy
          -> e -- ^ The element
          -> Array r ix e
singleton !c = unsafeMakeArray c oneIndex . const
{-# INLINE singleton #-}


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


instance ( Ragged L ix e
         , Construct L ix e
         , Source r ix e
         , Show e
         ) =>
         Show (Array r ix e) where
  show arr =
    "(Array " ++ showsTypeRep (typeRep (Proxy :: Proxy r)) " " ++
    showComp (getComp arr) ++ " (" ++
    (show (size arr)) ++ ")\n" ++
    show (unsafeMakeArray (getComp arr) (size arr) (evaluateAt arr) :: Array L ix e) ++ ")"
    where showComp Seq = "Seq"
          showComp Par = "Par"
          showComp c   = "(" ++ show c ++ ")"
