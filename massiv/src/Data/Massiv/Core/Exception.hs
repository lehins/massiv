{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Massiv.Core.Exception
  ( ImpossibleException(..)
  , InvalidRangeException(..)
  , throwImpossible
  , Uninitialized(..)
  , guardNumberOfElements
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


data InvalidRangeException where
  NegativeValue :: (Num a, Show a) => a -> InvalidRangeException
  NonPositiveValue :: (Num a, Show a) => a -> InvalidRangeException
  InvalidLowerBound :: (Num a, Show a) => a -> Bool -> a -> InvalidRangeException

instance Exception InvalidRangeException

instance Show InvalidRangeException where
  showsPrec n =
    \case
      NegativeValue v -> showsPrecWrapped n (("NegativeValue " ++) . showsPrec 1 v)
      NonPositiveValue v -> showsPrecWrapped n (("NonPositiveValue " ++) . showsPrec 1 v)
      InvalidLowerBound v incl b ->
        let gte =
              if incl
                then "at least"
                else "greater than"
         in showsPrecWrapped
              n
              (("InvalidLowerBound " ++) .
               showsPrec 1 v . ((" expected " ++ gte ++ ": ") ++) . showsPrec 1 b)
