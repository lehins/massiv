{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Compute
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Compute
  ( Mutable(..)
  , computeS
  , computeP
  , unsafeComputeP
  ) where

import           Control.Monad.Primitive    (PrimMonad (..))
import           Control.Monad.ST           (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Manifest
import           System.IO.Unsafe           (unsafePerformIO)



class Manifest r ix e => Mutable r ix e where
  data MArray s r ix e :: *

  unsafeThaw :: PrimMonad m =>
                Array r ix e -> m (MArray (PrimState m) r ix e)

  unsafeFreeze :: PrimMonad m =>
                  MArray (PrimState m) r ix e -> m (Array r ix e)

  unsafeNew :: PrimMonad m =>
               ix -> m (MArray (PrimState m) r ix e)

  unsafeLinearRead :: PrimMonad m =>
                      MArray (PrimState m) r ix e -> Int -> m e

  unsafeLinearWrite :: PrimMonad m =>
                       MArray (PrimState m) r ix e -> Int -> e -> m ()

  computeSeq :: Load r' ix => Array r' ix e -> Array r ix e
  computeSeq !arr = runST $ do
    mArr <- unsafeNew (size arr)
    loadS arr (unsafeLinearWrite mArr)
    unsafeFreeze mArr
  {-# INLINE computeSeq #-}

  computePar :: Load r' ix => Array r' ix e -> IO (Array r ix e)
  computePar !arr = do
    mArr <- unsafeNew (size arr)
    loadP arr (unsafeLinearWrite mArr)
    unsafeFreeze mArr
  {-# INLINE computePar #-}


computeAsSeq
  :: (Load r' ix, Mutable r ix e)
  => r -> Array r' ix e -> Array r ix e
computeAsSeq _ arr = computeSeq arr
{-# INLINE computeAsSeq #-}

computeAsPar
  :: (Load r' ix, Mutable r ix e)
  => r -> Array r' ix e -> IO (Array r ix e)
computeAsPar _ arr = computePar arr
{-# INLINE computeAsPar #-}


computeS
  :: (Load r' ix, Mutable r ix e)
  => r -> Array r' ix e -> Array M ix e
computeS r = toManifest . computeAsSeq r
{-# INLINE computeS #-}


computeP
  :: (Load r' ix, Mutable r ix e)
  => r -> Array r' ix e -> IO (Array M ix e)
computeP r arr = fmap toManifest (computeAsPar r arr)
{-# INLINE computeP #-}


unsafeComputeP
  :: (Load r' ix, Mutable r ix e)
  => r -> Array r' ix e -> Array M ix e
unsafeComputeP r arr = toManifest (unsafePerformIO (computeAsPar r arr))
{-# INLINE unsafeComputeP #-}


-- imapMaybeS :: forall r ix v e b.
--            (Iterator RowMajor ix, Source r ix e, VG.Vector v b)
--         => V v -> (ix -> e -> Maybe b) -> Array r ix e -> Array M DIM1 b
-- imapMaybeS _ f arr = do
--   MArray vLen (VG.unsafeIndex vector)
--   where
--     !(vLen, vData) = ifoldr RowMajor predAcc (0, []) arr
--     predAcc ix v !acc@(k, xs) = case f ix v of
--                                   Nothing -> acc
--                                   Just x  -> (k+1, x:xs)
--     {-# INLINE predAcc #-}
--     vector :: v b
--     !vector = VG.create generateArray
--     generateArray :: ST s (VG.Mutable v s b)
--     generateArray = do
--       mv <- MVG.unsafeNew vLen
--       _ <- loopM 0 (<vLen) (+1) vData $ \ !k (x:xs) ->
--         MVG.unsafeWrite mv k x >> return xs
--       return mv
--     {-# INLINE generateArray #-}
-- {-# INLINE imapMaybeS #-}


-- mapMaybeS
--   :: forall r ix v e b.
--      (Foldable (Array r ix), Source r ix e, VG.Vector v b)
--   => V v -> (e -> Maybe b) -> Array r ix e -> Array M DIM1 b
-- mapMaybeS _ f arr = do
--   MArray vLen (VG.unsafeIndex vector)
--   where
--     !(vLen, vData) = foldr' predAcc (0, []) arr
--     predAcc v !acc@(k, xs) = case f v of
--                                Nothing -> acc
--                                Just x  -> (k+1, x:xs)
--     {-# INLINE predAcc #-}
--     vector :: v b
--     !vector = VG.create generateArray
--     generateArray :: ST s (VG.Mutable v s b)
--     generateArray = do
--       mv <- MVG.unsafeNew vLen
--       _ <- loopM 0 (<vLen) (+1) vData $ \ !k (x:xs) ->
--         MVG.unsafeWrite mv k x >> return xs
--       return mv
--     {-# INLINE generateArray #-}
-- {-# INLINE mapMaybeS #-}



-- imapMaybeIxS :: forall r ix v e b.
--            (Iterator RowMajor ix, Source r ix e, VG.Vector v ix, VG.Vector v b)
--         => (ix -> e -> Maybe b) -> Array r ix e -> (v ix, v b)
-- imapMaybeIxS f arr = runST generateArrays
--   where
--     !(vLen, vData) = ifoldr RowMajor predAcc (0, []) arr
--     predAcc ix v !acc@(k, xs) = case f ix v of
--                                   Nothing -> acc
--                                   Just x  -> (k+1, (ix, x):xs)
--     {-# INLINE predAcc #-}
--     generateArrays :: (VG.Vector v ix, VG.Vector v b) => ST s (v ix, v b)
--     generateArrays = do
--       mvIx <- MVG.unsafeNew vLen
--       mvVal <- MVG.unsafeNew vLen
--       _ <- loopM 0 (<vLen) (+1) vData $ \ !k ((ix, x):xs) -> do
--         MVG.unsafeWrite mvIx k ix
--         MVG.unsafeWrite mvVal k x
--         return xs
--       vIx <- VG.unsafeFreeze mvIx
--       vVal <- VG.unsafeFreeze mvVal
--       return (vIx, vVal)
--     {-# INLINE generateArrays #-}
-- {-# INLINE imapMaybeIxS #-}
