{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Massiv.Core.Exception
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Core.Exception
  ( ImpossibleException(..)
  , throwImpossible
  , throwEither
  , Uninitialized(..)
  , guardNumberOfElements
  , Exception(..)
  , SomeException
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Data.Massiv.Core.Index.Internal

#if !MIN_VERSION_exceptions(0, 10, 3)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)

-- | Orphan instance in "massiv"
instance MonadThrow (ST s) where
  throwM = unsafeIOToST . throwIO
#endif


newtype ImpossibleException =
  ImpossibleException SomeException
  deriving (Show)

throwImpossible :: Exception e => e -> a
throwImpossible = throw . ImpossibleException . toException
{-# NOINLINE throwImpossible #-}

throwEither :: Either SomeException a -> a
throwEither =
  \case
    Left exc -> throw exc
    Right res -> res
{-# INLINE throwEither #-}

instance Exception ImpossibleException where
  displayException (ImpossibleException exc) =
    "<massiv> ImpossibleException (" ++
    displayException exc ++
    "): Either one of the unsafe functions was used or it is a bug in the library. " ++
    "In latter case please report this error."

-- | An error that gets thrown when an unitialized element of a boxed array gets accessed. Can only
-- happen when array was constructed with `unsafeNew`.
data Uninitialized = Uninitialized deriving Show

instance Exception Uninitialized where
  displayException Uninitialized = "Array element is uninitialized"


-- | Throw `SizeElementsMismatchException` whenever number of elements in both sizes do
-- not match.
--
-- @since 0.3.5
guardNumberOfElements :: (MonadThrow m, Index ix, Index ix') => Sz ix -> Sz ix' -> m ()
guardNumberOfElements sz sz' =
  unless (totalElem sz == totalElem sz') $ throwM $ SizeElementsMismatchException sz sz'
{-# INLINE guardNumberOfElements #-}

