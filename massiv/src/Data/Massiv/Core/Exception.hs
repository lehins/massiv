{-# LANGUAGE GADTs #-}
module Data.Massiv.Core.Exception
  ( ImpossibleException(..)
  , throwImpossible
  , Uninitialized(..)
  ) where

import           Control.Exception


newtype ImpossibleException =
  ImpossibleException SomeException
  deriving (Show)

throwImpossible :: Exception e => e -> a
throwImpossible = throw . ImpossibleException . toException

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


