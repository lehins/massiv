{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
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
  , compute
  , computeAs
  , computeSource
  , computeOn
  , computeAsOn
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
  , unsafeGenerateArray
  , unsafeGenerateArrayP
  ) where

import           Control.Monad.Primitive             (PrimMonad (..))
import           Control.Monad.ST                    (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed           (delay)
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Ops.Map           (iforM_)
import           Data.Array.Massiv.Scheduler
import           System.IO.Unsafe                    (unsafePerformIO)
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


compute :: (Load r' ix e, Mutable r ix e) => Array r' ix e -> Array r ix e
compute !arr =
  case getComp arr of
    Seq        -> loadMutableS arr
    ParOn wIds -> unsafePerformIO $ loadMutableOnP wIds arr
{-# INLINE compute #-}


computeAs :: (Load r' ix e, Mutable r ix e) => r -> Array r' ix e -> Array r ix e
computeAs _ = compute
{-# INLINE computeAs #-}


computeSource :: forall r' r ix e . (Source r' ix e, Mutable r ix e)
              => Array r' ix e -> Array r ix e
computeSource arr =
  fromMaybe (compute $ delay arr) $ fmap (\Refl -> arr) (eqT :: Maybe (r' :~: r))
{-# INLINE computeSource #-}



computeOn :: (Load r' ix e, Mutable r ix e) =>
             Comp -> Array r' ix e -> Array r ix e
computeOn comp = compute . setComp comp
{-# INLINE computeOn #-}

computeAsOn :: (Load r' ix e, Mutable r ix e) =>
               r' -> Comp -> Array r' ix e -> Array r ix e
computeAsOn _ = computeOn
{-# INLINE computeAsOn #-}


copy :: Mutable r ix e => Array r ix e -> Array r ix e
copy = compute . delay
{-# INLINE copy #-}


-- | Cast over Array representation
gcastArr :: forall r' r ix e. (Typeable r, Typeable r')
       => Array r' ix e -> Maybe (Array r ix e)
gcastArr arr = fmap (\Refl -> arr) (eqT :: Maybe (r :~: r'))


-- | /O(n)/ - conversion between manifest types, unless source and result arrays
-- are of the same representation, in which case it is an /O(1)/ operation.
convert :: (Manifest r' ix e, Mutable r ix e)
        => Array r' ix e -> Array r ix e
convert arr =
  fromMaybe (compute $ toManifest arr) (gcastArr arr)
{-# INLINE convert #-}


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
  splitWork_ wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
    loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
      submitRequest scheduler $
        JobRequest $
        iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !k !ix ->
          unsafeLinearWrite marr k (f ix)
    submitRequest scheduler $
      JobRequest $
      iterLinearM_ sz slackStart totalLength 1 (<) $ \ !k !ix ->
        unsafeLinearWrite marr k (f ix)
  unsafeFreeze (ParOn wIds) marr
{-# INLINE unsafeGenerateArrayP #-}

