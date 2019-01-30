{-# LANGUAGE GADTs #-}
module Data.Massiv.Core.Exception
  ( ImpossibleException(..)
  , throwImpossible
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


data ShapeException where
  RowTooShortError :: ShapeException -- TODO: add row length
  RowTooLongError :: ShapeException -- TODO: add row length

