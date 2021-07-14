{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Massiv.Core.Exception
-- Copyright   : (c) Alexey Kuleshevich 2019-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Exception
  ( throwImpossible
  , throwEither
  , Uninitialized(..)
  , guardNumberOfElements
  , module Primal.Exception
  ) where

import Primal.Exception
import Control.Monad
import Data.Massiv.Core.Index.Internal

-- | Throw an impossible error.
--
-- @since 0.5.6
throwImpossible :: (HasCallStack, Exception e) => e -> a
throwImpossible exc = error msg
  where
    msg =
      "<massiv> ImpossibleException (" ++
      displayException exc ++
      "): Either one of the unsafe functions was used or it is a bug in the library. " ++
      "In latter case please report this error."
{-# NOINLINE throwImpossible #-}

-- | Throw an error on `Left` or produce the result on `Right`. Exception type is lost, so
-- do not expect to be able to catch it as such. Stick to `IO` if you need exception control
-- flow.
--
-- @since 0.5.6
throwEither :: HasCallStack => Either SomeException a -> a
throwEither = raiseLeftImprecise
{-# INLINE throwEither #-}
{-# DEPRECATED throwEither "in favor of `raiseLeftImprecise`" #-}

-- | An error that gets thrown when an unitialized element of a boxed array gets accessed. Can only
-- happen when array was constructed with `unsafeNew`.
data Uninitialized = Uninitialized deriving Show

instance Exception Uninitialized where
  displayException Uninitialized = "Array element is uninitialized"


-- | Throw `SizeElementsMismatchException` whenever number of elements in both sizes do
-- not match.
--
-- @since 0.3.5
guardNumberOfElements :: (Raises m, Index ix, Index ix') => Sz ix -> Sz ix' -> m ()
guardNumberOfElements sz sz' =
  unless (totalElem sz == totalElem sz') $ raiseM $ SizeElementsMismatchException sz sz'
{-# INLINE guardNumberOfElements #-}

