{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
#if __GLASGOW_HASKELL__ >= 800
  {-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
#endif
-- |
-- Module      : Data.Massiv.Core
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core
  ( Array(List, unList)
  , Elt
  , EltRepr
  , Construct
  , Source
  , Load(..)
  , Size
  , Slice
  , OuterSlice(outerLength)
  , InnerSlice
  , Manifest
  , Mutable
  , Ragged(..)
  , Nested(..)
  , NestedStruct
  , L(..)
  , LN
  , ListItem
#if __GLASGOW_HASKELL__ >= 800
  , Comp(Seq, Par, ParOn)
  , pattern Par -- already exported above and only needed for Haddock
#else
  , Comp(..)
  , pattern Par
#endif
  , module Data.Massiv.Core.Index
  , elemsCount
  , isEmpty
  ) where

import           Data.Massiv.Core.Common hiding (unsafeGenerateM)
import           Data.Massiv.Core.List
import           Data.Massiv.Core.Index

-- | /O(1)/ - Get the number of elements in the array
elemsCount :: Size r ix e => Array r ix e -> Int
elemsCount = totalElem . size
{-# INLINE elemsCount #-}

-- | /O(1)/ - Check if array has no elements.
isEmpty :: Size r ix e => Array r ix e -> Bool
isEmpty !arr = 0 == elemsCount arr
{-# INLINE isEmpty #-}
