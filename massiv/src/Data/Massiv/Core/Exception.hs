{-# LANGUAGE GADTs #-}
module Data.Massiv.Core.Exception
  ( ImpossibleException(..)
  , throwImpossible
  , Uninitialized(..)
  , guardNumberOfElements
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Data.Massiv.Core.Index.Internal

newtype ImpossibleException =
  ImpossibleException SomeException
  deriving (Show)

throwImpossible :: Exception e => e -> a
throwImpossible = throw . ImpossibleException . toException
{-# NOINLINE throwImpossible #-}

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


guardNumberOfElements :: (MonadThrow m, Index ix, Index ix') => Sz ix -> Sz ix' -> m ()
guardNumberOfElements sz sz' = do
  unless (totalElem sz == totalElem sz) $ throwM $ SizeElementsMismatchException sz sz'
{-# INLINE guardNumberOfElements #-}
