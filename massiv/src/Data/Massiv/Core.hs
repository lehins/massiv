-- |
-- Module      : Data.Massiv.Core
-- Copyright   : (c) Alexey Kuleshevich 2018-2025
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
module Data.Massiv.Core (
  Array (LArray),
  List (..),
  Vector,
  MVector,
  Matrix,
  MMatrix,
  Load (iterArrayLinearST_, iterArrayLinearWithSetST_),
  Stream (..),
  Source,
  PrefIndex (..),
  Size,
  Shape (..),
  LengthHint (..),
  StrideLoad (..),
  Manifest,
  Mutable,
  Ragged,
  L (..),
  ListItem,
  Scheduler,
  SchedulerWS,
  Strategy,
  Comp (Seq, Par, Par', ParOn, ParN),
  getComp,
  setComp,
  appComp,
  WorkerStates,
  initWorkerStates,
  scheduleWork,
  scheduleWork_,
  module Data.Massiv.Core.Index,

  -- * Numeric
  FoldNumeric,
  Numeric,
  NumericFloat,

  -- * Exceptions
  MonadThrow (..),
  IndexException (..),
  SizeException (..),
  ShapeException (..),
  module Data.Massiv.Core.Exception,

  -- * Stateful Monads
  MonadUnliftIO,
  MonadIO (liftIO),
  PrimMonad (PrimState),
) where

import Control.Scheduler (SchedulerWS, initWorkerStates)
import Data.Massiv.Core.Common
import Data.Massiv.Core.Exception
import Data.Massiv.Core.Index
import Data.Massiv.Core.List
import Data.Massiv.Core.Operations (FoldNumeric, Numeric, NumericFloat)

-- | Append computation strategy using `Comp`'s `Monoid` instance.
--
-- @since 0.6.0
appComp :: Strategy r => Comp -> Array r ix e -> Array r ix e
appComp comp arr = setComp (comp <> getComp arr) arr
{-# INLINEABLE appComp #-}
