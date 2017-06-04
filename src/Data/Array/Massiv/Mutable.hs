{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Array.Massiv.Mutable
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Mutable
  ( Mutable(..)
  , Target(..)
  , compute
  , computeAs
  , computeOn
  , computeAsOn
  , copy
  , convert
  , convertAs
  , gcastArr
  , loadTargetS
  , loadTargetOnP
  , unsafeRead
  , unsafeWrite
  , sequenceP
  , sequenceOnP
  ) where

import           Control.Monad.Primitive             (PrimMonad (..))
import           Control.Monad.ST                    (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed           (delay)
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Ops.Map           (iforM_)
import           Data.Array.Massiv.Scheduler
import           System.IO.Unsafe                    (unsafePerformIO)
--import Control.DeepSeq
import           Data.Maybe                          (fromMaybe)
import           Data.Typeable


class Manifest r ix e => Mutable r ix e where
  data MArray s r ix e :: *

  -- | Get the size of a mutable array.
  msize :: MArray s r ix e -> ix

  unsafeThaw :: PrimMonad m =>
                Array r ix e -> m (MArray (PrimState m) r ix e)

  unsafeFreeze :: PrimMonad m =>
                  Comp -> MArray (PrimState m) r ix e -> m (Array r ix e)

  unsafeNew :: PrimMonad m =>
               ix -> m (MArray (PrimState m) r ix e)

  unsafeLinearRead :: PrimMonad m =>
                      MArray (PrimState m) r ix e -> Int -> m e

  unsafeLinearWrite :: PrimMonad m =>
                       MArray (PrimState m) r ix e -> Int -> e -> m ()


class Mutable r ix e => Target r ix e where

  unsafeTargetRead :: PrimMonad m =>
                      MArray (PrimState m) r ix e -> Int -> m e
  unsafeTargetRead = unsafeLinearRead
  {-# INLINE unsafeTargetRead #-}

  unsafeTargetWrite :: PrimMonad m =>
                       MArray (PrimState m) r ix e -> Int -> e -> m ()
  unsafeTargetWrite = unsafeLinearWrite
  {-# INLINE unsafeTargetWrite #-}


loadTargetS :: (Load r' ix e, Target r ix e) =>
               Array r' ix e -> Array r ix e
loadTargetS !arr =
  runST $ do
    mArr <- unsafeNew (size arr)
    loadS arr (unsafeTargetRead mArr) (unsafeTargetWrite mArr)
    unsafeFreeze Seq mArr
{-# INLINE loadTargetS #-}

loadTargetOnP :: (Load r' ix e, Target r ix e) =>
                 [Int] -> Array r' ix e -> IO (Array r ix e)
loadTargetOnP wIds !arr = do
  mArr <- unsafeNew (size arr)
  loadP wIds arr (unsafeTargetRead mArr) (unsafeTargetWrite mArr)
  unsafeFreeze (ParOn wIds) mArr
{-# INLINE loadTargetOnP #-}


compute :: (Load r' ix e, Target r ix e) => Array r' ix e -> Array r ix e
compute !arr =
  case getComp arr of
    Seq        -> loadTargetS arr
    ParOn wIds -> unsafePerformIO $ loadTargetOnP wIds arr
{-# INLINE compute #-}


computeAs :: (Load r' ix e, Target r ix e) => r -> Array r' ix e -> Array r ix e
computeAs _ = compute
{-# INLINE computeAs #-}


computeOn :: (Load r' ix e, Target r ix e) =>
             Comp -> Array r' ix e -> Array r ix e
computeOn comp = compute . setComp comp
{-# INLINE computeOn #-}

computeAsOn :: (Load r' ix e, Target r ix e) =>
               r' -> Comp -> Array r' ix e -> Array r ix e
computeAsOn _ = computeOn
{-# INLINE computeAsOn #-}


copy :: Target r ix e => Array r ix e -> Array r ix e
copy = compute . delay
{-# INLINE copy #-}


-- | Cast over Array representation
gcastArr :: forall r' r ix e. (Typeable r, Typeable r')
       => Array r' ix e -> Maybe (Array r ix e)
gcastArr arr = fmap (\Refl -> arr) (eqT :: Maybe (r :~: r'))

convert :: (Target r' ix e, Target r ix e)
        => Array r' ix e -> Array r ix e
convert arr =
  fromMaybe (compute $ delay arr) (gcastArr arr)
{-# INLINE convert #-}


convertAs :: (Target r' ix e, Target r ix e, Typeable ix, Typeable e)
          => r -> Array r' ix e -> Array r ix e
convertAs _ = convert
{-# INLINE convertAs #-}



unsafeRead :: (Mutable r ix e, PrimMonad m) =>
               MArray (PrimState m) r ix e -> ix -> m e
unsafeRead !marr !ix = unsafeLinearRead marr (toLinearIndex (msize marr) ix)
{-# INLINE unsafeRead #-}

unsafeWrite :: (Mutable r ix e, PrimMonad m) =>
               MArray (PrimState m) r ix e -> ix -> e -> m ()
unsafeWrite !marr !ix = unsafeLinearWrite marr (toLinearIndex (msize marr) ix)
{-# INLINE unsafeWrite #-}


sequenceOnP :: (Source r1 ix (IO e), Mutable r ix e) =>
               [Int] -> Array r1 ix (IO e) -> IO (Array r ix e)
sequenceOnP wIds !arr = do
  resArrM <- unsafeNew (size arr)
  scheduler <- makeScheduler wIds
  iforM_ arr $ \ !ix action ->
    submitRequest scheduler $ JobRequest (action >>= unsafeWrite resArrM ix)
  waitTillDone scheduler
  unsafeFreeze (getComp arr) resArrM
{-# INLINE sequenceOnP #-}


sequenceP :: (Source r1 ix (IO e), Mutable r ix e) => Array r1 ix (IO e) -> IO (Array r ix e)
sequenceP = sequenceOnP []
{-# INLINE sequenceP #-}



-- sequenceOnP' :: (NFData e, Source r1 ix (IO e), Mutable r ix e) =>
--                [Int] -> Array r1 ix (IO e) -> IO (Array r ix e)
-- sequenceOnP' wIds !arr = do
--   resArrM <- unsafeNew (size arr)
--   scheduler <- makeScheduler wIds
--   iforM_ arr $ \ !ix action ->
--     submitRequest scheduler $ JobRequest $ do
--       res <- action
--       res `deepseq` unsafeWrite resArrM ix res
--   waitTillDone scheduler
--   unsafeFreeze resArrM
-- {-# INLINE sequenceOnP' #-}


-- sequenceP' :: (NFData e, Source r1 ix (IO e), Mutable r ix e)
--            => Array r1 ix (IO e) -> IO (Array r ix e)
-- sequenceP' = sequenceOnP' []
-- {-# INLINE sequenceP' #-}


