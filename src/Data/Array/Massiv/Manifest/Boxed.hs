{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
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
  , generateM
  -- * Mapping
  , mapM
  , imapM
  -- * Conversion
  , fromVectorBoxed
  , toVectorBoxed
  -- * Evaluation
  , computeBoxedS
  , computeBoxedP
  , deepseq
  , deepseqP
  ) where

import           Control.DeepSeq
import           Control.Monad                       (void)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Ops
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import           Data.Array.Massiv.Scheduler
import           Data.Foldable                       (Foldable (..))
import qualified Data.Vector                         as V
import qualified Data.Vector.Mutable                 as MV
import           GHC.Base                            (build)
import           Prelude                             hiding (mapM)
import           System.IO.Unsafe                    (unsafePerformIO)

data B = B

data instance Array B ix e = BArray { bSize :: !ix
                                    , bData :: !(V.Vector e)
                                    } deriving Eq

instance Index ix => Massiv B ix e where
  size = bSize
  {-# INLINE size #-}

  makeArray sz = BArray sz . makeBoxedVector sz
  {-# INLINE makeArray #-}

instance Index ix => Source B ix e where
  unsafeLinearIndex (BArray _ v) = V.unsafeIndex v
  {-# INLINE unsafeLinearIndex #-}


instance Index ix => Shape B ix e where
  type R B = M

  unsafeReshape !sz !arr = arr { bSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance (Index ix, Index (Lower ix)) => Slice B ix e where

  (!?>) !arr = (toManifest arr !?>)
  {-# INLINE (!?>) #-}

  (<!?) !arr = (toManifest arr <!?)
  {-# INLINE (<!?) #-}


instance Index ix => Manifest B ix e where

  unsafeLinearIndexM (BArray _ v) = V.unsafeIndex v
  {-# INLINE unsafeLinearIndexM #-}


instance Index ix => Mutable B ix e where
  data MArray s B ix e = MBArray ix (V.MVector s e)

  msize (MBArray sz _) = sz
  {-# INLINE msize #-}

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


-- | Loading a Boxed array in parallel only make sense if it's elements are
-- fully evaluated into NF.
instance (Index ix, NFData e) => Target B ix e where

  unsafeTargetWrite !mv !i e = e `deepseq` unsafeLinearWrite mv i e
  {-# INLINE unsafeTargetWrite #-}


instance Index ix => Load B ix e where
  loadS (BArray sz v) _ uWrite =
    iterLinearM_ sz 0 (totalElem sz) 1 (<) $ \ !i _ ->
      uWrite i (V.unsafeIndex v i)
  {-# INLINE loadS #-}
  loadP wIds (BArray sz v) _ uWrite = do
    void $
      splitWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          submitRequest scheduler $
          JobRequest $
          iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !i _ ->
            uWrite i $ V.unsafeIndex v i
        submitRequest scheduler $
          JobRequest $
          iterLinearM_ sz slackStart totalLength 1 (<) $ \ !i _ ->
            uWrite i $ V.unsafeIndex v i
  {-# INLINE loadP #-}

instance (Index ix, NFData e) => NFData (Array B ix e) where
  rnf (BArray sz v) = sz `deepseq` v `deepseq` ()


-- | Row-major folding over a Boxed array.
instance Index ix => Foldable (Array B ix) where
  foldl = lazyFoldlS
  {-# INLINE foldl #-}
  foldl' = foldlS
  {-# INLINE foldl' #-}
  foldr = foldrFB
  {-# INLINE foldr #-}
  foldr' = foldrS
  {-# INLINE foldr' #-}
  null (BArray sz _) = totalElem sz == 0
  {-# INLINE null #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}



computeBoxedS :: (Load r ix e, Target B ix e) => Array r ix e -> Array B ix e
computeBoxedS = loadTargetS
{-# INLINE computeBoxedS #-}


computeBoxedP :: (Load r ix e, Target B ix e) => Array r ix e -> Array B ix e
computeBoxedP = unsafePerformIO . loadTargetOnP []
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
mapM f = imapM (const f)
{-# INLINE mapM #-}

imapM :: (Source r ix a, Monad m) =>
  (ix -> a -> m b) -> Array r ix a -> m (Array B ix b)
imapM f arr = do
  let !sz = size arr
  v <- V.generateM (totalElem sz) $ \ !i ->
         let !ix = fromLinearIndex sz i
             !e = unsafeIndex arr ix
         in f ix e
  return $ BArray sz v
{-# INLINE imapM #-}


-- | Parallel version of `deepseq`: fully evaluate all elements of a boxed array in
-- parallel, while returning back the second argument.
deepseqP :: (Index ix, NFData a) => Array B ix a -> b -> b
deepseqP (BArray sz v) b =
  unsafePerformIO
    ((splitWork [] sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          submitRequest scheduler $
          JobRequest $
          loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k ->
            V.unsafeIndex v k `deepseq` return ()
        submitRequest scheduler $
          JobRequest $
          loopM_ slackStart (< totalLength) (+ 1) $ \ !k ->
            V.unsafeIndex v k `deepseq` return ()) >>
     return (deepseq sz b))
{-# INLINE deepseqP #-}
