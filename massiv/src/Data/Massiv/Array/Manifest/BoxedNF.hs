{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Massiv.Array.Manifest.Boxed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Manifest.BoxedNF
  ( N (..)
  , Array(..)
  , deepseqArray
  , deepseqArrayP
  , vectorFromArray
  , vectorToArray
  , castVectorToArray
  ) where

import           Control.DeepSeq                     (NFData (..), deepseq)
import           Control.Monad.ST                    (runST)
import           Data.Massiv.Array.Delayed.Internal  (eq)
import           Data.Massiv.Array.Manifest.Internal (M, toManifest)
import           Data.Massiv.Array.Mutable
import           Data.Massiv.Core
import           Data.Massiv.Core.Scheduler
import           Data.Massiv.Core.List
import qualified Data.Primitive.Array                as A
import qualified Data.Vector                         as VB
import qualified Data.Vector.Mutable                 as VB
import           GHC.Exts                            (IsList (..))
import           Prelude                             hiding (mapM)
import           System.IO.Unsafe                    (unsafePerformIO)

-- | Array representation for Boxed elements. This structure is element and
-- spine strict, and elements are always in Normal Form (NF), therefore `NFData`
-- instance is required.
data N = N deriving Show

type instance EltRepr N ix = M

data instance Array N ix e = NArray { nComp :: Comp
                                    , nSize :: !ix
                                    , nData :: {-# UNPACK #-} !(A.Array e)
                                    }

instance (Index ix, NFData e) => NFData (Array N ix e) where
  rnf (NArray comp sz arr) = -- comp `deepseq` sz `deepseq` a `seq` ()
    case comp of
      Seq        -> deepseqArray sz arr ()
      ParOn wIds -> deepseqArrayP wIds sz arr ()


instance (Index ix, NFData e, Eq e) => Eq (Array N ix e) where
  (==) = eq (==)
  {-# INLINE (==) #-}


instance (Index ix, NFData e) => Construct N ix e where
  size = nSize
  {-# INLINE size #-}

  getComp = nComp
  {-# INLINE getComp #-}

  setComp c arr = arr { nComp = c }
  {-# INLINE setComp #-}

  unsafeMakeArray Seq          !sz f = unsafeGenerateArray sz f
  unsafeMakeArray (ParOn wIds) !sz f = unsafeGenerateArrayP wIds sz f
  {-# INLINE unsafeMakeArray #-}

instance (Index ix, NFData e) => Source N ix e where
  unsafeLinearIndex (NArray _ _ a) = A.indexArray a
  {-# INLINE unsafeLinearIndex #-}


instance (Index ix, NFData e) => Shape N ix e where

  unsafeReshape !sz !arr = arr { nSize = sz }
  {-# INLINE unsafeReshape #-}

  unsafeExtract !sIx !newSz !arr = unsafeExtract sIx newSz (toManifest arr)
  {-# INLINE unsafeExtract #-}


instance ( NFData e
         , Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt N ix e ~ Array M (Lower ix) e
         ) =>
         OuterSlice N ix e where
  unsafeOuterSlice arr = unsafeOuterSlice (toManifest arr)
  {-# INLINE unsafeOuterSlice #-}

instance ( NFData e
         , Index ix
         , Index (Lower ix)
         , Elt M ix e ~ Array M (Lower ix) e
         , Elt N ix e ~ Array M (Lower ix) e
         ) =>
         InnerSlice N ix e where
  unsafeInnerSlice arr = unsafeInnerSlice (toManifest arr)
  {-# INLINE unsafeInnerSlice #-}


instance (Index ix, NFData e) => Manifest N ix e where

  unsafeLinearIndexM (NArray _ _ a) = A.indexArray a
  {-# INLINE unsafeLinearIndexM #-}


uninitialized :: a
uninitialized = error "Data.Array.Massiv.Manifest.BoxedNF: uninitialized element"


instance (Index ix, NFData e) => Mutable N ix e where
  data MArray s N ix e = MNArray !ix {-# UNPACK #-} !(A.MutableArray s e)

  msize (MNArray sz _) = sz
  {-# INLINE msize #-}

  unsafeThaw (NArray _ sz a) = MNArray sz <$> A.unsafeThawArray a
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (MNArray sz ma) = NArray comp sz <$> A.unsafeFreezeArray ma
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = MNArray sz <$> A.newArray (totalElem sz) uninitialized
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (MNArray _ ma) i = A.readArray ma i
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (MNArray _ ma) i e = e `deepseq` A.writeArray ma i e
  {-# INLINE unsafeLinearWrite #-}


deepseqArray :: (Index ix, NFData a) => ix -> A.Array a -> b -> b
deepseqArray sz arr b =
  iter 0 (totalElem sz) 1 (<) b $ \ !i acc -> A.indexArray arr i `deepseq` acc
{-# INLINE deepseqArray #-}


deepseqArrayP :: (Index ix, NFData a) => [Int] -> ix -> A.Array a -> b -> b
deepseqArrayP wIds sz arr b =
  unsafePerformIO $ do
    divideWork_ wIds sz $ \ !scheduler !chunkLength !totalLength !slackStart -> do
      loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
        scheduleWork scheduler $
        loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k ->
          A.indexArray arr k `deepseq` return ()
      scheduleWork scheduler $
        loopM_ slackStart (< totalLength) (+ 1) $ \ !k ->
          A.indexArray arr k `deepseq` return ()
    return b
{-# INLINE deepseqArrayP #-}


vectorFromArray :: Index ix => ix -> A.Array a -> VB.Vector a
vectorFromArray sz arr = runST $ do
  marr <- A.unsafeThawArray arr
  VB.unsafeFreeze $ VB.MVector 0 (totalElem sz) marr
{-# INLINE vectorFromArray #-}


vectorToArray :: VB.Vector a -> A.Array a
vectorToArray v =
  runST $ do
    VB.MVector start len marr <- VB.unsafeThaw v
    marr' <-
      if start == 0
        then return marr
        else A.cloneMutableArray marr start len
    A.unsafeFreezeArray marr'
{-# INLINE vectorToArray #-}


-- | Cast a Boxed Vector into an Array, but only if it wasn't previously sliced.
castVectorToArray :: VB.Vector a -> Maybe (A.Array a)
castVectorToArray v =
  runST $ do
    VB.MVector start _ marr <- VB.unsafeThaw v
    if start == 0
      then Just <$> A.unsafeFreezeArray marr
      else return Nothing
{-# INLINE castVectorToArray #-}



instance (NFData e, IsList (Array L ix e), Load L ix e, Construct L ix e) =>
         IsList (Array N ix e) where
  type Item (Array N ix e) = Item (Array L ix e)
  fromList xs = compute (fromList xs :: Array L ix e)
  {-# INLINE fromList #-}
  toList = toListArray
  {-# INLINE toList #-}
