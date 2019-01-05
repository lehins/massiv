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
  , Load(loadArray)
  , Source
  , Extract
  , StrideLoad(..)
  , Slice
  , OuterSlice
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
  ) where

import           Data.Massiv.Core.Common hiding (indexWith, unsafeGenerateM)
import           Data.Massiv.Core.List
import           Data.Massiv.Core.Index

