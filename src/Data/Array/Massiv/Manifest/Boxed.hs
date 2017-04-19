{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Massiv.Manifest.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Manifest.Boxed
  ( B (..)
  -- * Creation
  , makeBoxedArray
  , generateM
  -- * Mapping
  , mapM
  , imapM
  -- * Conversion
  , fromVectorBoxed
  , toVectorBoxed
  , fromListsBoxed
  -- * Evaluation
  , computeBoxedS
  , computeBoxedP
  , deepseq
  , deepseqP
  ) where

import           Control.DeepSeq
import           Control.Monad                       (void)
import           Control.Monad.ST                    (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import           Data.Array.Massiv.Scheduler
import           Data.Maybe                          (listToMaybe)
import qualified Data.Vector                         as V
import qualified Data.Vector.Mutable                 as MV
import           Prelude                             hiding (mapM)
import           System.IO.Unsafe                    (unsafePerformIO)

data B = B

data instance Array B ix e = BArray { bSize :: !ix
                                    , bData :: !(V.Vector e)
                                    } deriving Eq

instance Index ix => Massiv B ix where
  size = bSize
  {-# INLINE size #-}


instance Index ix => Source B ix e where
  unsafeLinearIndex (BArray _ v) = V.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Manifest B ix e


instance (NFData e, Manifest B ix e) => Mutable B ix e where
  data MArray s B ix e = MBArray ix (V.MVector s e)

  unsafeThaw (BArray sz v) = MBArray sz <$> V.unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze (MBArray sz v) = BArray sz <$> V.unsafeFreeze v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MBArray sz <$> MV.unsafeNew (totalElem sz)
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MBArray _sz v) i = MV.unsafeRead v i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MBArray _sz v) i = MV.unsafeWrite v i
  {-# INLINE unsafeLinearWrite #-}

  computeSeq !arr = runST $ do
    mArr <- unsafeNew (size arr)
    loadS arr (unsafeLinearRead mArr) (unsafeLinearWrite' mArr)
    unsafeFreeze mArr
  {-# INLINE computeSeq #-}

  computePar !arr = do
    mArr <- unsafeNew (size arr)
    loadP arr (unsafeLinearRead mArr) (unsafeLinearWrite' mArr)
    unsafeFreeze mArr
  {-# INLINE computePar #-}


instance Index ix => Load B ix where
  loadS (BArray sz v) _ unsafeWrite =
    iterLinearM_ sz 0 (totalElem sz) 1 (<) $ \ !i _ ->
      unsafeWrite i (V.unsafeIndex v i)
  {-# INLINE loadS #-}
  loadP (BArray sz v) _ unsafeWrite = do
    void $
      splitWork sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          submitRequest scheduler $
          JobRequest 0 $
          iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !i _ ->
            unsafeWrite i $ V.unsafeIndex v i
        submitRequest scheduler $
          JobRequest 0 $
          iterLinearM_ sz slackStart totalLength 1 (<) $ \ !i _ ->
            unsafeWrite i $ V.unsafeIndex v i
  {-# INLINE loadP #-}

instance (Index ix, NFData e) => NFData (Array B ix e) where
  rnf (BArray sz v) = sz `deepseq` v `deepseq` ()


-- | Create a boxed array, where all elements are not evaluated. Use `deepseq`
-- or `deepseqP` to get elements to NF.
makeBoxedArray :: Index ix => ix -> (ix -> e) -> Array B ix e
makeBoxedArray sz f =
  BArray sz $ V.generate (totalElem sz) (f . fromLinearIndex sz)
{-# INLINE makeBoxedArray #-}


fromListsBoxed :: [[e]] -> Array B DIM2 e
fromListsBoxed !ls =
  if all (== n) (map length ls)
    then BArray (m, n) (V.fromList $ concat ls)
    else error "fromListsVG:Inner lists are of different lengths."
  where -- TODO: check dims
    (m, n) = (length ls, maybe 0 length $ listToMaybe ls)
{-# INLINE fromListsBoxed #-}


computeBoxedS :: (Load r' ix, Mutable B ix e) => Array r' ix e -> Array B ix e
computeBoxedS = computeSeq
{-# INLINE computeBoxedS #-}


computeBoxedP :: (Load r' ix, Mutable B ix e) => Array r' ix e -> Array B ix e
computeBoxedP = unsafePerformIO . computePar
{-# INLINE computeBoxedP #-}


fromVectorBoxed :: Index ix => ix -> V.Vector e -> Array B ix e
fromVectorBoxed sz v = BArray { bSize = sz, bData = v }
{-# INLINE fromVectorBoxed #-}


toVectorBoxed :: Array B ix e -> V.Vector e
toVectorBoxed = bData
{-# INLINE toVectorBoxed #-}


generateM :: (Index ix, Monad m) =>
  ix -> (ix -> m a) -> m (Array B ix a)
generateM sz f =
  BArray sz <$> V.generateM (totalElem sz) (f . fromLinearIndex sz)
{-# INLINE generateM #-}


mapM :: (Source r ix a, Monad m) =>
  (a -> m b) -> Array r ix a -> m (Array B ix b)
mapM f arr = do
  let !sz = size arr
  v <- V.generateM (totalElem sz) (f . unsafeLinearIndex arr)
  return $ BArray sz v
{-# INLINE mapM #-}

imapM :: (Source r ix a, Monad m) =>
  (ix -> a -> m b) -> Array r ix a -> m (Array B ix b)
imapM f arr = do
  let !sz = size arr
  v <- V.generateM (totalElem sz) $ \ !i ->
         let !ix = fromLinearIndex sz i
         in f ix (unsafeIndex arr ix)
  return $ BArray sz v
{-# INLINE imapM #-}


-- | Parallel version of `deepseq`: fully evaluate all elements of a boxed array in
-- parallel, while returning back the second argument.
deepseqP :: (Index ix, NFData a) => Array B ix a -> b -> b
deepseqP (BArray sz v) b =
  unsafePerformIO
    ((splitWork sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          submitRequest scheduler $
          JobRequest 0 $
          loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k ->
            V.unsafeIndex v k `deepseq` return ()
        submitRequest scheduler $
          JobRequest 0 $
          loopM_ slackStart (< totalLength) (+ 1) $ \ !k ->
            V.unsafeIndex v k `deepseq` return ()) >>
     return (deepseq sz b))
{-# INLINE deepseqP #-}
