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
module Data.Array.Massiv.Manifest.BoxedStrict
  ( B (..)
  , Array(..)
  -- * Creation
  -- , generateM
  -- -- * Mapping
  -- , mapM
  -- , imapM
  -- -- * Conversion
  -- , fromVectorBoxed
  -- , toVectorBoxed
  -- -- * Evaluation
  -- , computeBoxedS
  -- , computeBoxedP
  -- , deepseq
  -- , deepseqP
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Control.Monad                       (void)
import           Control.Monad.ST                    (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Common.Shape
import           Data.Array.Massiv.Manifest.Internal
import           Data.Array.Massiv.Mutable
import           Data.Array.Massiv.Ops.Fold
import           Data.Array.Massiv.Scheduler
import           Data.Foldable                       (Foldable (..))
import qualified Data.Primitive.Array                as A
import           GHC.Base                            (build)
import           Prelude                             hiding (mapM)
import           System.IO.Unsafe                    (unsafePerformIO)

data B = B deriving Show

data instance Array B ix e = BArray { bComp :: !Comp
                                    , bSize :: !ix
                                    , bData :: {-# UNPACK #-} !(A.Array e)
                                    }

instance (Index ix, NFData e) => NFData (Array B ix e) where
  rnf arr =
    case getComp arr of
      Seq        -> arr `deepseqArray` ()
      Par        -> arr `deepseqArrayP` ()
      ParOn wIds -> deepseqArrayOnP wIds arr ()



instance Index ix => Construct B ix e where
  size = bSize
  {-# INLINE size #-}

  getComp = bComp
  {-# INLINE getComp #-}

  setComp c arr = arr { bComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray Seq          !sz f = unsafeGenerateArray sz f
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
  {-# INLINE unsafeMakeArray #-}

instance Index ix => Source B ix e where
  unsafeLinearIndex (BArray _ _ a) = A.indexArray a
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

  unsafeLinearIndexM (BArray _ _ a) = A.indexArray a
  {-# INLINE unsafeLinearIndexM #-}


uninitialized :: a
uninitialized = error "Data.Array.Massiv.Manifest.BoxedStrict: uninitialized element"


instance Index ix => Mutable B ix e where
  data MArray s B ix e = MBArray !ix {-# UNPACK #-} !(A.MutableArray s e)

  msize (MBArray sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (BArray _ sz a) = MBArray sz <$> A.unsafeThawArray a
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MBArray sz ma) = BArray comp sz <$> A.unsafeFreezeArray ma
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MBArray sz <$> A.newArray (totalElem sz) uninitialized
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MBArray _ ma) i = A.readArray ma i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MBArray _ ma) i e = e `seq` A.writeArray ma i e
  {-# INLINE unsafeLinearWrite #-}


-- | Loading a Boxed array in parallel only make sense if it's elements are
-- fully evaluated into NF.
instance (Index ix, NFData e) => Target B ix e where

  unsafeTargetWrite !mv !i e = e `deepseq` unsafeLinearWrite mv i e
  {-# INLINE unsafeTargetWrite #-}


-- instance Index ix => Load B ix e where
--   loadS (BArray _ sz v) _ uWrite =
--     iterLinearM_ sz 0 (totalElem sz) 1 (<) $ \ !i _ ->
--       uWrite i (V.unsafeIndex v i)
--   {-# INLINE loadS #-}
--   loadP wIds (BArray _ sz v) _ uWrite = do
--     void $
--       splitWork wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
--         loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
--           submitRequest scheduler $
--           JobRequest $
--           iterLinearM_ sz start (start + chunkLength) 1 (<) $ \ !i _ ->
--             uWrite i $ V.unsafeIndex v i
--         submitRequest scheduler $
--           JobRequest $
--           iterLinearM_ sz slackStart totalLength 1 (<) $ \ !i _ ->
--             uWrite i $ V.unsafeIndex v i
--   {-# INLINE loadP #-}


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
  null (BArray _ sz _) = totalElem sz == 0
  {-# INLINE null #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}
  length = totalElem . size
  {-# INLINE length #-}
  toList arr = build (\ c n -> foldrFB c n arr)
  {-# INLINE toList #-}


deepseqArray :: (Index ix, NFData a) => Array B ix a -> b -> b
deepseqArray (BArray _ sz a) b =
  iter 0 (totalElem sz) 1 (<) b $ \ !i acc -> A.indexArray a i `deepseq` acc
{-# INLINE deepseqArray #-}


-- | Parallel version of `deepseq`: fully evaluate all elements of a boxed array in
-- parallel, while returning back the second argument.
deepseqArrayP :: (Index ix, NFData a) => Array B ix a -> b -> b
deepseqArrayP = deepseqArrayOnP []
{-# INLINE deepseqArrayP #-}


deepseqArrayOnP :: (Index ix, NFData a) => [Int] -> Array B ix a -> b -> b
deepseqArrayOnP wIds (BArray _ sz v) b =
  unsafePerformIO $ do
    splitWork_ wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
        submitRequest scheduler $
        JobRequest $
        loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k ->
          A.indexArray v k `deepseq` return ()
      submitRequest scheduler $
        JobRequest $
        loopM_ slackStart (< totalLength) (+ 1) $ \ !k ->
          A.indexArray v k `deepseq` return ()
    return b
{-# INLINE deepseqArrayOnP #-}

