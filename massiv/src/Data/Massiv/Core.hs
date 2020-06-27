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
  , Vector
  , MVector
  , Matrix
  , MMatrix
  , Elt
  , Construct
  , Load(R, loadArrayM, defaultElement)
  , Stream(..)
  , Source
  , Resize
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
  , Comp(Seq, Par, Par', ParOn, ParN)
  , WorkerStates
  , initWorkerStates
  , module Data.Massiv.Core.Index
  -- * Exceptions
  , MonadThrow(..)
  , throw
  , IndexException(..)
  , SizeException(..)
  , ShapeException(..)
  , module Data.Massiv.Core.Exception
  -- * Stateful Monads
  , MonadUnliftIO
  , MonadIO(liftIO)
  , PrimMonad(PrimState)
  ) where

import Control.Scheduler (initWorkerStates)
import Data.Massiv.Core.Common
import Data.Massiv.Core.Index
import Data.Massiv.Core.List
import Data.Massiv.Core.Exception

