{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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
  , fromListSeq
  , fromListsSeq
  , unsafeLinearWrite'
  ) where

import           Control.DeepSeq
import           Control.Monad                       (void)
import           Control.Monad.Primitive             (PrimMonad (..))
import           Control.Monad.ST                    (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Manifest.Internal
import           Data.Maybe                          (listToMaybe)



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
    loadS arr (unsafeLinearRead mArr) (unsafeLinearWrite mArr)
    unsafeFreeze mArr
  {-# INLINE computeSeq #-}

  computePar :: Load r' ix => Array r' ix e -> IO (Array r ix e)
  computePar !arr = do
    mArr <- unsafeNew (size arr)
    loadP arr (unsafeLinearRead mArr) (unsafeLinearWrite mArr)
    unsafeFreeze mArr
  {-# INLINE computePar #-}


unsafeLinearWrite' :: (NFData e, PrimMonad m, Mutable r ix e) =>
                      MArray (PrimState m) r ix e -> Int -> e -> m ()
unsafeLinearWrite' mv i e = e `deepseq` unsafeLinearWrite mv i e
{-# INLINE unsafeLinearWrite' #-}


fromListSeq :: Mutable r DIM1 e => [e] -> Array r DIM1 e
fromListSeq xs =
  runST $ do
    let !k = length xs
    mArr <- unsafeNew k
    let loadList _ []      = return []
        loadList !i (y:ys) = unsafeLinearWrite mArr i y >> return ys
        {-# INLINE loadList #-}
    void $ loopM 0 (< k) (+ 1) xs loadList
    unsafeFreeze mArr
{-# INLINE fromListSeq #-}


fromListsSeq :: Mutable r DIM2 e => [[e]] -> Array r DIM2 e
fromListsSeq xs =
  runST $ do
    let !(m, n) = (length xs, maybe 0 length $ listToMaybe xs)
    mArr <- unsafeNew (m, n)
    let loadCol !i !j (z:zs) = unsafeLinearWrite mArr (i * n + j) z >> return zs
        loadCol i _ _ =
          error $
          "fromListsSeq: Inner list on row: " ++
          show i ++ " was shorter then the first one."
        {-# INLINE loadCol #-}
    let loadRow _ [] = return []
        loadRow !i (y:ys) = loopM 0 (< n) (+ 1) y (loadCol i) >> return ys
        {-# INLINE loadRow #-}
    void $ loopM 0 (< m) (+ 1) xs loadRow
    unsafeFreeze mArr
{-# INLINE fromListsSeq #-}


-- fromListsSeq' :: Mutable r DIM2 e => [[e]] -> Array r DIM2 e
-- fromListsSeq' xs =
--   runST $ do
--     let !(m, n) = (length xs, maybe 0 length $ listToMaybe xs)
--     mArr <- unsafeNew (m, n)
--     let loadCol _ !j []
--           | j == n = return ()
--           | otherwise =
--             error "fromListsSeq: One of inner lists was of different length."
--         loadCol !i !j (z:zs) = do
--           unsafeLinearWrite mArr (i * n + j) z
--           loadCol i (j + 1) zs
--     let loadRow _ []      = return ()
--         loadRow !i (y:ys) = do
--           loadCol i 0 y
--           loadRow (i + 1) ys
--     loadRow 0 xs
--     unsafeFreeze mArr
-- {-# INLINE fromListsSeq' #-}
