{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Mutable
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.Mutable
  ( compute
  , computeAs
  , computeSource
  , copy
  , convert
  , convertAs
  , gcastArr
  , loadMutableS
  , loadMutableOnP
  , unsafeRead
  , unsafeWrite
  , sequenceP
  , sequenceOnP
  , fromRaggedArray
  , fromRaggedArray'
  , unsafeGenerateArray
  , unsafeGenerateArrayP
  ) where

import           Control.Exception
import           Control.Monad.Primitive             (PrimMonad (..))
import           Control.Monad.ST                    (runST)
import           Data.Massiv.Array.Delayed.Internal  (delay)
import           Data.Massiv.Array.Manifest.Internal
import           Data.Massiv.Array.Ops.Map           (iforM_)
import           Data.Massiv.Core.Common
import           Data.Massiv.Core.List
import           Data.Massiv.Core.Scheduler
import           Data.Maybe                          (fromMaybe)
import           Data.Typeable
import           System.IO.Unsafe                    (unsafePerformIO)



loadMutableS :: (Load r' ix e, Mutable r ix e) =>
                Array r' ix e -> Array r ix e
loadMutableS !arr =
  runST $ do
    mArr <- unsafeNew (size arr)
    loadS arr (unsafeLinearRead mArr) (unsafeLinearWrite mArr)
    unsafeFreeze Seq mArr
{-# INLINE loadMutableS #-}

loadMutableOnP :: (Load r' ix e, Mutable r ix e) =>
                 [Int] -> Array r' ix e -> IO (Array r ix e)
loadMutableOnP wIds !arr = do
  mArr <- unsafeNew (size arr)
  loadP wIds arr (unsafeLinearRead mArr) (unsafeLinearWrite mArr)
  unsafeFreeze (ParOn wIds) mArr
{-# INLINE loadMutableOnP #-}

-- | Ensure that Array is computed, i.e. represented with concrete elements in memory, hence is the
-- `Mutable` type class restriction. Use `setComp` if you'd like to change computation strategy
-- before calling @compute@
compute :: (Load r' ix e, Mutable r ix e) => Array r' ix e -> Array r ix e
compute !arr =
  case getComp arr of
    Seq        -> loadMutableS arr
    ParOn wIds -> unsafePerformIO $ loadMutableOnP wIds arr
{-# INLINE compute #-}

-- | Just as `compute`, but let's you supply resulting representation type as an argument.
computeAs :: (Load r' ix e, Mutable r ix e) => r -> Array r' ix e -> Array r ix e
computeAs _ = compute
{-# INLINE computeAs #-}


-- | This is just like `compute`, but can be applied to `Source` arrays and will be a noop if
-- resulting type is the same as the input.
computeSource :: forall r' r ix e . (Source r' ix e, Mutable r ix e)
              => Array r' ix e -> Array r ix e
computeSource arr =
  fromMaybe (compute $ delay arr) $ fmap (\Refl -> arr) (eqT :: Maybe (r' :~: r))
{-# INLINE computeSource #-}


-- | /O(n)/ - Make a copy of an Array.
copy :: Mutable r ix e => Array r ix e -> Array r ix e
copy = compute . toManifest
{-# INLINE copy #-}


-- | Cast over Array representation
gcastArr :: forall r' r ix e. (Typeable r, Typeable r')
       => Array r' ix e -> Maybe (Array r ix e)
gcastArr arr = fmap (\Refl -> arr) (eqT :: Maybe (r :~: r'))


-- | /O(n)/ - conversion between manifest types, except when source and result arrays
-- are of the same representation, in which case it is an /O(1)/ operation.
convert :: (Manifest r' ix e, Mutable r ix e)
        => Array r' ix e -> Array r ix e
convert arr =
  fromMaybe (compute $ toManifest arr) (gcastArr arr)
{-# INLINE convert #-}

-- | Just as `convert`, but let's you supply resulting representation type as an argument.
convertAs :: (Mutable r' ix e, Mutable r ix e, Typeable ix, Typeable e)
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
  withScheduler_ wIds $ \scheduler ->
    iforM_ arr $ \ !ix action ->
      scheduleWork scheduler $ action >>= unsafeWrite resArrM ix
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

-- | Convert a ragged array into a usual rectangular shaped one.
fromRaggedArray :: (Ragged r' ix e, Mutable r ix e) =>
                   Array r' ix e -> Either ShapeError (Array r ix e)
fromRaggedArray arr = unsafePerformIO $ do
  let sz = edgeSize arr
  mArr <- unsafeNew sz
  let loadWith using =
        loadRagged using (unsafeLinearWrite mArr) 0 (totalElem sz) (tailDim sz) arr
  try $ case getComp arr of
          Seq -> loadWith id >> unsafeFreeze (getComp arr) mArr
          ParOn ss -> do
            withScheduler_ ss (loadWith . scheduleWork)
            unsafeFreeze (getComp arr) mArr
{-# INLINE fromRaggedArray #-}

-- | Same as `fromRaggedArray`, but will throw an error if its shape is not
-- rectangular.
fromRaggedArray' :: (Ragged r' ix e, Mutable r ix e) =>
                    Array r' ix e -> Array r ix e
fromRaggedArray' arr =
  case fromRaggedArray arr of
    Left RowTooShortError -> error "Not enough elements in a row"
    Left RowTooLongError  -> error "Too many elements in a row"
    Right resArr -> resArr
{-# INLINE fromRaggedArray' #-}


-- | Create an array sequentially using mutable interface
unsafeGenerateArray :: Mutable r ix e => ix -> (ix -> e) -> Array r ix e
unsafeGenerateArray !sz f = runST $ do
  marr <- unsafeNew sz
  iterLinearM_ sz 0 (totalElem sz) 1 (<) $ \ !k !ix ->
    unsafeLinearWrite marr k (f ix)
  unsafeFreeze Seq marr
{-# INLINE unsafeGenerateArray #-}


-- | Create an array in parallel using mutable interface
unsafeGenerateArrayP :: Mutable r ix e => [Int] -> ix -> (ix -> e) -> Array r ix e
unsafeGenerateArrayP wIds !sz f = unsafePerformIO $ do
  marr <- unsafeNew sz
  divideWork_ wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
    loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
      scheduleWork scheduler $
        iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !k !ix ->
          unsafeLinearWrite marr k (f ix)
    scheduleWork scheduler $
      iterLinearM_ sz slackStart totalLength 1 (<) $ \ !k !ix ->
        unsafeLinearWrite marr k (f ix)
  unsafeFreeze (ParOn wIds) marr
{-# INLINE unsafeGenerateArrayP #-}

