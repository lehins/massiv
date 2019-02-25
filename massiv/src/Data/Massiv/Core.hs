{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
#if __GLASGOW_HASKELL__ > 820
{-# OPTIONS_GHC -Wno-deplicate-exports #-}
#endif
-- |
-- Module      : Data.Massiv.Core
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
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
  , Load(loadArrayM)
  , Source
  , Extract
  , StrideLoad(..)
  , Slice
  , OuterSlice
  , InnerSlice
  , Manifest
  , Mutable
  , Ragged
  , Nested(..)
  , NestedStruct
  , L(..)
  , LN
  , ListItem
  , Comp(Seq, Par, ParOn, ParN)
  , module Data.Massiv.Core.Index
  , MonadUnliftIO
  ) where

import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import           Data.Massiv.Core.Index

