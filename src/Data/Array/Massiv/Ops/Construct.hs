{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Array.Massiv.Ops.Construct
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Ops.Construct
  ( -- * Construction
    makeArray1D
  , makeArray2D
  , makeArray3D
  -- * Enumeration
  , range
  , rangeStep
  , enumFromN
  , enumFromStepN
  -- * Conversion
  , fromListsS
  , fromListSeq
  , fromListsSeq
  ) where

import           Control.Monad              (void)
import           Control.Monad.ST           (runST)
import           Data.Array.Massiv.Common
import           Data.Array.Massiv.Delayed
import           Data.Array.Massiv.Manifest
import           Data.Array.Massiv.Mutable
import           Data.Maybe                 (listToMaybe)


makeArray1D :: Int -> (Int -> e) -> Array D DIM1 e
makeArray1D = makeArray
{-# INLINE makeArray1D #-}


makeArray2D :: (Int, Int) -> ((Int, Int) -> e) -> Array D DIM2 e
makeArray2D = makeArray
{-# INLINE makeArray2D #-}

makeArray3D :: (Int, Int, Int) -> ((Int, Int, Int) -> e) -> Array D DIM3 e
makeArray3D = makeArray
{-# INLINE makeArray3D #-}




-- | Create a Vector with a range of @Int@s incremented by 1.
-- @range k0 k1 == rangeStep k0 k1 1@
--
-- >>> toList $ range 1 6
-- [1,2,3,4,5]
-- >>> toList $ range (-2) 3
-- [-2,-1,0,1,2]
range :: Int -> Int -> Array D DIM1 Int
range !k0 !k1 = makeArray1D (max 0 (k1 - k0)) (+ k0)
{-# INLINE range #-}


rangeStep :: Int -> Int -> Int -> Array D DIM1 Int
rangeStep !k0 !k1 !step = makeArray1D ((k1 - k0) `div` step) (\ !i -> k0 + i*step)
{-# INLINE rangeStep #-}


-- |
--
-- >>> toList $ enumFromN 5 3
-- [5,6,7]
enumFromN :: Num e => e -> Int -> Array D DIM1 e
enumFromN !s !k = DArray k $ \ !i -> fromIntegral i + s
{-# INLINE enumFromN #-}

-- |
--
-- >>> toList $ enumFromStepN 1 0.1 5
-- [1.0,1.1,1.2,1.3,1.4]
enumFromStepN :: Num e => e -> e -> Int -> Array D DIM1 e
enumFromStepN !s !step !k = DArray k $ \ !i -> fromIntegral i * step + s
{-# INLINE enumFromStepN #-}



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


fromListsSeq :: Mutable r DIM2 e => r -> [[e]] -> Array r DIM2 e
fromListsSeq _ xs =
  runST $ do
    let !(m, n) = (length xs, maybe 0 length $ listToMaybe xs)
    mArr <- unsafeNew (m, n)
    let loadCol !i !j (z:zs) = unsafeLinearWrite mArr (i * n + j) z >> return zs
        loadCol i _ _ =
          error $
          "fromListsSeq: Inner list on row: " ++
          show i ++ " was shorter then the first one."
        {-# INLINE loadCol #-}
    let loadRow _ []      = return []
        loadRow !i (y:ys) = loopM 0 (< n) (+ 1) y (loadCol i) >> return ys
        {-# INLINE loadRow #-}
    void $ loopM 0 (< m) (+ 1) xs loadRow
    unsafeFreeze mArr
{-# INLINE fromListsSeq #-}


fromListsS :: Mutable r DIM2 e => r -> [[e]] -> Array M DIM2 e
fromListsS r xs = toManifest (fromListsSeq r xs)

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
